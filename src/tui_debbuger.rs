use anyhow::Result;
use crossterm::event::{self, KeyEventKind};
use ratatui::{style::Stylize, widgets::Paragraph, DefaultTerminal};

pub fn start_tui_debugger() -> Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;

    let app_result = run(terminal);
    ratatui::restore();

    app_result
}

fn run(mut terminal: DefaultTerminal) -> Result<()> {
    loop {
        terminal.draw(|frame| {
            let greeting = Paragraph::new("Hello TUI Debugger! (press 'q' to quit)")
                .white()
                .on_dark_gray();
            frame.render_widget(greeting, frame.area());
        })?;

        if let event::Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press && key.code == event::KeyCode::Char('q') {
                return Ok(());
            }
        }
    }
}
