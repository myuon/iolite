use std::{
    collections::HashMap,
    io::BufWriter,
    path::Path,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Direction, Layout, Rect},
    style::Stylize,
    symbols::border,
    text::Line,
    widgets::{Block, Padding, Paragraph, Widget, Wrap},
    DefaultTerminal, Frame,
};

use crate::compiler::{
    self,
    byte_code_emitter::emit_disassemble,
    runtime::{ControlFlow, Runtime},
};

struct Debugger {
    runtime: Arc<Mutex<Runtime>>,
    exit: bool,
    mode: String,
    next_instruction: Option<String>,
    focus: usize,
}

pub fn start_tui_debugger(filepath: String) -> Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;

    let mut debugger = Debugger {
        runtime: Arc::new(Mutex::new(Runtime::new(1024, vec![], HashMap::new()))),
        exit: false,
        mode: "launched".to_string(),
        next_instruction: None,
        focus: 0,
    };

    let path = Path::new(&filepath);
    let cwd = path.parent().unwrap().to_str().unwrap().to_string();
    let filename = path.file_name().unwrap().to_str().unwrap().to_string();

    debugger.launch(cwd, filename)?;

    let app_result = debugger.run(&mut terminal);
    ratatui::restore();

    app_result
}

impl Debugger {
    fn launch(&mut self, cwd: String, source_file: String) -> Result<()> {
        let source_file = Path::new(&cwd)
            .join(Path::new(&source_file))
            .to_str()
            .unwrap()
            .to_string();

        let mut compiler = compiler::Compiler::new();
        compiler.set_cwd(cwd);

        let main = "main".to_string();
        let source_code = std::fs::read_to_string(&source_file)?;

        compiler.parse_with_code(main.clone(), source_code.clone(), false)?;
        compiler.typecheck(main.clone())?;
        compiler.ir_code_gen(main.clone(), false)?;
        compiler.vm_code_gen()?;
        compiler.link()?;
        compiler.byte_code_gen()?;

        let program = compiler.result_codegen.as_ref().unwrap().buffer.clone();

        self.runtime.lock().unwrap().init(
            1024,
            program,
            std::path::Path::new(&source_file)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
            source_code,
        );

        Ok(())
    }

    fn next(&mut self) {
        let mut runtime = self.runtime.lock().unwrap();
        let flow = runtime.step(false, false).unwrap();

        match flow {
            ControlFlow::HitBreakpoint => {
                self.mode = "breakpoint".to_string();
            }
            ControlFlow::Finish => {
                self.mode = "finished".to_string();
            }
            ControlFlow::Continue => {}
        }

        self.next_instruction = Some(format!("{:?}", runtime.show_next_instruction()));
    }

    fn resume(&mut self) {
        self.mode = "running".to_string();
        let mut runtime = self.runtime.lock().unwrap();

        let flow = {
            let mut flow = ControlFlow::Continue;
            while matches!(flow, ControlFlow::Continue) {
                flow = runtime.step(false, false).unwrap();
            }

            flow
        };

        match flow {
            ControlFlow::HitBreakpoint => {
                self.mode = "breakpoint".to_string();
            }
            ControlFlow::Finish => {
                self.mode = "finished".to_string();
            }
            _ => (),
        }

        self.next_instruction = Some(format!("{:?}", runtime.show_next_instruction()));
    }

    fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_event()?;
        }

        Ok(())
    }

    fn exit(&mut self) {
        self.exit = true;
    }

    fn draw(&self, frame: &mut Frame) {
        frame.render_widget(self, frame.area());
    }

    fn handle_event(&mut self) -> Result<()> {
        match event::read()? {
            Event::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                self.handle_key_event(key_event);
            }
            _ => (),
        };

        Ok(())
    }

    fn handle_key_event(&mut self, key_event: KeyEvent) {
        match key_event.code {
            KeyCode::Char('q') => self.exit(),
            KeyCode::Char('r') => self.resume(),
            KeyCode::Char('n') => self.next(),
            KeyCode::Tab => self.focus = (self.focus + 1) % 2,
            _ => {}
        }
    }
}

impl Widget for &Debugger {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let outer_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(100)])
            .split(area);

        let vertical_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .margin(2)
            .split(outer_layout[0]);

        let horizontal_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(vertical_layout[0]);

        let title = Line::from(" TUI Debugger ").bold();
        let instructions = Line::from(vec![
            " Resume ".into(),
            "<R>".blue().into(),
            " Next ".into(),
            "<N>".blue().into(),
            " Quit ".into(),
            "<Q> ".blue().into(),
        ]);
        let block = Block::bordered()
            .title(title.centered())
            .title_bottom(instructions)
            .border_set(border::THICK);

        let runtime = self.runtime.lock().unwrap();

        Paragraph::new(format!(
            "[mode:{}] pc:0x{:x}, bp:0x{:x}, sp:0x{:x}{}",
            self.mode,
            runtime.pc,
            runtime.bp,
            runtime.sp,
            self.next_instruction
                .clone()
                .map(|t| format!(", next:{}", t))
                .unwrap_or(String::new())
        ))
        .block(block)
        .render(outer_layout[0], buf);

        // source_code block
        {
            let mut block = Block::bordered()
                .title(" Source Code ")
                .padding(Padding::left(4));
            if self.focus == 0 {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let source_code_lines = runtime
                .source_code
                .lines()
                .enumerate()
                .map(|(i, t)| format!("{:>4} | {}", i, t))
                .collect::<Vec<_>>();

            Paragraph::new(
                source_code_lines
                    .iter()
                    .map(|s| Line::raw(s.as_str()))
                    .collect::<Vec<_>>(),
            )
            .wrap(Wrap { trim: true })
            .block(block)
            .render(horizontal_layout[0], buf);
        }

        // disassemble block
        {
            let mut block = Block::bordered().title(" Disassemble ");
            if self.focus == 1 {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let mut writer = BufWriter::new(Vec::new());
            emit_disassemble(&mut writer, runtime.program.clone()).unwrap();

            Paragraph::new(
                String::from_utf8(writer.buffer().to_vec())
                    .unwrap()
                    .lines()
                    .map(|s| Line::raw(s))
                    .collect::<Vec<_>>(),
            )
            .block(block)
            .render(horizontal_layout[1], buf);
        }

        let bottom_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Fill(1), Constraint::Fill(1)])
            .split(vertical_layout[1]);

        // stack frame block
        {
            let block = Block::bordered().title(" Stack Frames ");

            let frames = runtime.get_stack_frames();

            let stack_trace = frames
                .iter()
                .map(|frame| format!("0x{:x}", frame))
                .collect::<Vec<_>>();

            Paragraph::new(
                stack_trace
                    .iter()
                    .map(|s| Line::raw(s.as_str()))
                    .collect::<Vec<_>>(),
            )
            .block(block)
            .render(bottom_layout[0], buf);
        }

        // stack block
        {
            let block = Block::bordered().title(" Stack ");
            let values = runtime.get_stack_values_from_top();

            Paragraph::new(
                values
                    .iter()
                    .map(|(addr, value)| Line::raw(format!("0x{:x} | {:?}", addr, value)))
                    .collect::<Vec<_>>(),
            )
            .block(block)
            .render(bottom_layout[1], buf);
        }
    }
}
