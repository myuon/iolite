use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::Stylize,
    symbols::border,
    text::Line,
    widgets::{Block, Paragraph, Widget},
    DefaultTerminal, Frame,
};

use crate::compiler::runtime::Runtime;

pub fn start_tui_debugger() -> Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;

    let mut debugger = Debugger {
        runtime: Arc::new(Mutex::new(Runtime::new(1024, vec![], HashMap::new()))),
        exit: false,
    };
    let app_result = debugger.run(&mut terminal);
    ratatui::restore();

    app_result
}

struct Debugger {
    runtime: Arc<Mutex<Runtime>>,
    exit: bool,
}

impl Debugger {
    fn run(&mut self, terminal: &mut DefaultTerminal) -> Result<()> {
        while !self.exit {
            terminal.draw(|frame| self.draw(frame))?;
            self.handle_event();
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
            KeyCode::Char('q') => self.exit = true,
            _ => {}
        }
    }
}

impl Widget for &Debugger {
    fn render(self, area: Rect, buf: &mut Buffer) {
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

        Paragraph::new("foobar").block(block).render(area, buf);
    }
}
