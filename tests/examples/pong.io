struct Position {
  x: int,
  y: int,
}

struct Direction {
  x: bool,
  y: bool,
}

struct Ball {
  widget: Frame,
  pos: Position,
  dir: Direction,
}

module Ball {
  fun build(w: int, h: int): Ball {
    let widget = Frame::default();
    widget.set_rectangle(0, 0, w, h);

    let pos = Position { x: 0, y: 0 };
    let dir = Direction { x: true, y: true };

    return Ball {
      widget: widget,
      pos: pos,
      dir: dir,
    };
  }
}

fun main() {
  let app = App::default();

  let window = Window::build(100, 200, 800, 600, "Pong!");

  let ball = Ball::build(40, 40);

  window.end();
  window.show();
  window.set_callback(fun (event: Event) {
    if (Event::KEYDOWN().! == event.!) {
      let key = app.event_key();

      if (Key::ESCAPE().! == key.!) {
        app.quit();
      }
    }

    return nil;
  });

  let paddle_pos = 320;

  app.run();
}
