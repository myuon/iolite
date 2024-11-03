use std::{
    collections::HashMap,
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

use crate::compiler::{self, runtime::Runtime};

pub fn start_tui_debugger(filepath: String) -> Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;

    let mut debugger = Debugger {
        runtime: Arc::new(Mutex::new(Runtime::new(1024, vec![], HashMap::new()))),
        exit: false,
    };

    let path = Path::new(&filepath);
    let cwd = path.parent().unwrap().to_str().unwrap().to_string();
    let filename = path.file_name().unwrap().to_str().unwrap().to_string();

    debugger.launch(cwd, filename)?;

    let app_result = debugger.run(&mut terminal);
    ratatui::restore();

    app_result
}

struct Debugger {
    runtime: Arc<Mutex<Runtime>>,
    exit: bool,
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

        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .margin(2)
            .split(outer_layout[0]);

        let title = Line::from(" TUI Debugger ").bold();
        let instructions = Line::from(vec![
            " Resume ".into(),
            "<R>".blue().into(),
            " Step ".into(),
            "<S>".blue().into(),
            " Quit ".into(),
            "<Q> ".blue().into(),
        ]);
        let block = Block::bordered()
            .title(title.centered())
            .title_bottom(instructions)
            .border_set(border::THICK);

        let runtime = self.runtime.lock().unwrap();

        Paragraph::new(format!(
            "pc:{}, bp:{}, sp:{}",
            runtime.pc, runtime.bp, runtime.sp
        ))
        .block(block)
        .render(outer_layout[0], buf);

        // source_code block
        {
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
            .block(
                Block::bordered()
                    .title(" Source Code ")
                    .padding(Padding::left(4)),
            )
            .render(layout[0], buf);
        }

        // disassemble block
        {
            Paragraph::new(" hogehoge piyopiyo")
                .block(Block::bordered().title(" Disassemble "))
                .render(layout[1], buf);
        }
    }
}
