use std::{
    collections::HashMap,
    io::{BufWriter, Write},
    path::Path,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use ratatui::{
    prelude::*,
    widgets::{Block, Padding, Paragraph, Wrap},
    DefaultTerminal,
};
use symbols::border;

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
    disassembled: String,
    exit: bool,
    mode: String,
    next_instruction: Option<String>,
    focus: DebuggerView,
    scrolls: HashMap<DebuggerView, u16>,
    labels: HashMap<usize, String>,
}

pub fn start_tui_debugger(filepath: String) -> Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;

    let mut debugger = Debugger {
        runtime: Arc::new(Mutex::new(Runtime::new(1024, vec![], HashMap::new()))),
        disassembled: String::new(),
        exit: false,
        mode: "launched".to_string(),
        next_instruction: None,
        focus: DebuggerView::SourceCode,
        scrolls: HashMap::new(),
        labels: HashMap::new(),
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

        let emitter = compiler.result_codegen.as_ref().unwrap();
        let program = emitter.buffer.clone();

        let mut buffer = vec![];
        {
            let mut writer = BufWriter::new(&mut buffer);
            emit_disassemble(&mut writer, program.clone())?;
            writer.flush()?;
        }

        self.disassembled = String::from_utf8(buffer)?;
        self.labels = emitter
            .labels
            .clone()
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<HashMap<_, _>>();

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
        if self.mode == "finished" {
            return;
        }

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

        let disassemble_scroll = self.scrolls.entry(DebuggerView::Disassemble).or_insert(0);
        if !self
            .disassembled
            .lines()
            .nth(*disassemble_scroll as usize)
            .unwrap()
            .starts_with(&format!("0x{:x}", runtime.pc))
        {
            for (i, line) in self.disassembled.lines().enumerate() {
                if line.starts_with(&format!("0x{:x}", runtime.pc)) {
                    *disassemble_scroll = i as u16;
                    break;
                }
            }
        }
    }

    fn resume(&mut self) {
        if self.mode == "finished" {
            return;
        }

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

        let disassemble_scroll = self.scrolls.entry(DebuggerView::Disassemble).or_insert(0);
        if !self
            .disassembled
            .lines()
            .nth(*disassemble_scroll as usize)
            .unwrap()
            .starts_with(&format!("0x{:x}", runtime.pc))
        {
            for (i, line) in self.disassembled.lines().enumerate() {
                if line.starts_with(&format!("0x{:x}", runtime.pc)) {
                    *disassemble_scroll = i as u16;
                    break;
                }
            }
        }
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
                .title(format!(
                    " Source Code [{},{}] ",
                    runtime.prev_source_map.0, runtime.prev_source_map.1
                ))
                .padding(Padding::left(1));
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

                        let source_map = runtime.prev_source_map;
                        lines.push(if start == source_map.0 && end <= source_map.1 {
                            text.black().on_blue()
                        } else {
                            text
                        });
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
            let mut block = Block::bordered()
                .title(format!(" Disassemble [0x{:x}] ", runtime.pc))
                .padding(Padding::left(2));
            if self.focus == DebuggerView::Disassemble {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let lines = self
                .disassembled
                .lines()
                .map(|t| {
                    let line = Line::raw(t.to_string());
                    if t.starts_with(&format!("0x{:x}:", runtime.pc)) {
                        line.on_blue().black()
                    } else {
                        line
                    }
                })
                .collect::<Vec<_>>();

            Paragraph::new(lines)
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
            .constraints(vec![
                Constraint::Fill(1),
                Constraint::Fill(1),
                Constraint::Fill(1),
            ])
            .split(vertical_layout[1]);

        let frames = runtime.get_stack_frames();

        // stack frame block
        {
            let mut block = Block::bordered().title(" Stack Frames ");
            if self.focus == DebuggerView::StackFrames {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let stack_trace = frames
                .iter()
                .enumerate()
                .map(|(i, frame)| {
                    format!("[{}] 0x{:x} {}", i, frame, {
                        let ip = runtime.called_ips.get(frames.len() - 1 - i);
                        if let Some(ip) = ip {
                            format!("#{}", self.labels.get(ip).unwrap_or(&"main".to_string()))
                        } else {
                            "<prepare>".to_string()
                        }
                    })
                })
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
                    .enumerate()
                    .map(|(i, (addr, value))| {
                        let mut line = Line::raw(format!("[{}] 0x{:x} | {:?}", i, addr, value));
                        if let Some(k) = frames.iter().position(|frame| frame == addr) {
                            line.push_span(Span::from(format!(" | #frame:[{}]", k)));

                            line.on_blue().black()
                        } else {
                            line
                        }
                    })
                    .collect::<Vec<_>>(),
            )
            .block(block)
            .scroll((
                self.scrolls.get(&DebuggerView::Stack).copied().unwrap_or(0),
                0,
            ))
            .render(bottom_layout[1], buf);
        }

        // labels
        {
            let mut block = Block::bordered().title(" Labels ");
            if self.focus == DebuggerView::Stack {
                block = block.yellow().border_set(border::DOUBLE);
            }

            let mut labels_iter = self.labels.iter().collect::<Vec<_>>();
            labels_iter.sort();

            let labels = labels_iter
                .into_iter()
                .map(|(addr, label)| Line::raw(format!("0x{:x} -> {}", addr, label)))
                .collect::<Vec<_>>();

            Paragraph::new(labels)
                .block(block)
                .scroll((
                    self.scrolls.get(&DebuggerView::Stack).copied().unwrap_or(0),
                    0,
                ))
                .render(bottom_layout[2], buf);
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
