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
  let paddle_x = 320;

  window.end();
  window.show();
  window.draw(fun () {
    Draw::set_draw_color(255, 255, 255);
    Draw::draw_rect(paddle_x, 540, 160, 20);

    return nil;
  });
  window.set_callback(fun (event: Event) {
    if (Event::KEYDOWN().! == event.!) {
      let key = app.event_key();

      if (Key::ESCAPE().! == key.!) {
        app.quit();
      }
    } else if (Event::MOVE().! == event.!) {
      let coords = app.event_coords();

      paddle_x = coords.x - 80;
    }

    return nil;
  });

  app.add_idle(fun () {
    // window.redraw();
    // App::sleep(0.016);

    return nil;
  });
  app.run();
}
