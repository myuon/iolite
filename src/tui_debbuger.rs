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
    text::{Line, Span, Text},
    widgets::{Block, Padding, Paragraph, Widget, Wrap},
    DefaultTerminal, Frame,
};

use crate::compiler::{
    self,
    byte_code_emitter::emit_disassemble,
    lexer::{Lexeme, Lexer, Token},
    runtime::{ControlFlow, Runtime},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum DebuggerView {
    SourceCode,
    Disassemble,
    StackFrames,
    Stack,
}

struct Debugger {
    runtime: Arc<Mutex<Runtime>>,
    exit: bool,
    mode: String,
    next_instruction: Option<String>,
    focus: DebuggerView,
    scrolls: HashMap<DebuggerView, u16>,
}

pub fn start_tui_debugger(filepath: String) -> Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;

    let mut debugger = Debugger {
        runtime: Arc::new(Mutex::new(Runtime::new(1024, vec![], HashMap::new()))),
        exit: false,
        mode: "launched".to_string(),
        next_instruction: None,
        focus: DebuggerView::SourceCode,
        scrolls: HashMap::new(),
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
            KeyCode::Char(' ') => self.next(),
            KeyCode::Tab => {
                self.focus = match self.focus {
                    DebuggerView::SourceCode => DebuggerView::Disassemble,
                    DebuggerView::Disassemble => DebuggerView::StackFrames,
                    DebuggerView::StackFrames => DebuggerView::Stack,
                    DebuggerView::Stack => DebuggerView::SourceCode,
                }
            }
            KeyCode::Up => {
                let scroll = self.scrolls.entry(self.focus).or_insert(0);
                *scroll = scroll.saturating_sub(1);
            }
            KeyCode::Down => {
                let scroll = self.scrolls.entry(self.focus).or_insert(0);
                *scroll += 1;
            }
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
            "<N/SPACE>".blue().into(),
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
                .unwrap_or(String::new()),
        ))
        .block(block)
        .render(outer_layout[0], buf);

        // source_code block
        {
            let mut block = Block::bordered()
                .title(" Source Code ")
                .padding(Padding::left(4));
            if self.focus == DebuggerView::SourceCode {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let mut lexer = Lexer::new("main".to_string(), runtime.source_code.clone());
            let tokens = lexer.run().unwrap();
            let keywords = get_token_type_and_positions(tokens);

            let chars = runtime.source_code.chars().collect::<Vec<_>>();
            let mut line = 0;
            let mut lines = vec![];
            let mut text = Line::raw(format!("|{:>3}| ", line));

            let mut current = 0;
            for (token_type, start, end) in keywords {
                while current < start {
                    if let Some(at) = chars[current..start].iter().position(|c| *c == '\n') {
                        text.push_span(Span::from(
                            chars[current..current + at].iter().collect::<String>(),
                        ));
                        current += at + 1;
                        line += 1;

                        lines.push(text);
                        text = Line::raw(format!("|{:>3}| ", line));
                    } else {
                        break;
                    }
                }

                text.push_span(Span::from(chars[current..start].iter().collect::<String>()));
                text.push_span(Span::from(chars[start..end].iter().collect::<String>()).fg(
                    match token_type.as_str() {
                        "ident" => crossterm::style::Color::White,
                        "string" => crossterm::style::Color::Green,
                        "numeric" => crossterm::style::Color::Blue,
                        "comment" => crossterm::style::Color::DarkGrey,
                        _ => crossterm::style::Color::Magenta,
                    },
                ));

                current = end;
            }
            text.push_span(Span::from(chars[current..].iter().collect::<String>()));
            lines.push(text);

            Paragraph::new(lines)
                .wrap(Wrap { trim: true })
                .block(block)
                .scroll((
                    self.scrolls
                        .get(&DebuggerView::SourceCode)
                        .copied()
                        .unwrap_or(0),
                    0,
                ))
                .render(horizontal_layout[0], buf);
        }

        // disassemble block
        {
            let mut block = Block::bordered().title(" Disassemble ");
            if self.focus == DebuggerView::Disassemble {
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
            .scroll((
                self.scrolls
                    .get(&DebuggerView::Disassemble)
                    .copied()
                    .unwrap_or(0),
                0,
            ))
            .render(horizontal_layout[1], buf);
        }

        let bottom_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Fill(1), Constraint::Fill(1)])
            .split(vertical_layout[1]);

        // stack frame block
        {
            let mut block = Block::bordered().title(" Stack Frames ");
            if self.focus == DebuggerView::StackFrames {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let mut frames = runtime.get_stack_frames();
            frames.reverse();

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
            .scroll((
                self.scrolls
                    .get(&DebuggerView::StackFrames)
                    .copied()
                    .unwrap_or(0),
                0,
            ))
            .render(bottom_layout[0], buf);
        }

        // stack block
        {
            let mut block = Block::bordered().title(" Stack ");
            if self.focus == DebuggerView::Stack {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let mut values = runtime.get_stack_values_from_top();
            values.reverse();

            Paragraph::new(
                values
                    .iter()
                    .map(|(addr, value)| Line::raw(format!("0x{:x} | {:?}", addr, value)))
                    .collect::<Vec<_>>(),
            )
            .block(block)
            .scroll((
                self.scrolls.get(&DebuggerView::Stack).copied().unwrap_or(0),
                0,
            ))
            .render(bottom_layout[1], buf);
        }
    }
}

fn get_token_type_and_positions(tokens: Vec<Token>) -> Vec<(String, usize, usize)> {
    let mut result = vec![];
    for token in tokens {
        let token_type = match token.lexeme {
            Lexeme::Ident(_) => "ident",
            Lexeme::String(_) => "string",
            Lexeme::Integer(_) => "numeric",
            Lexeme::Float(_) => "numeric",
            Lexeme::Comment(_) => "comment",
            _ => "keyword",
        };
        result.push((
            token_type.to_string(),
            token.span.start.unwrap(),
            token.span.end.unwrap(),
        ));
    }

    result
}
