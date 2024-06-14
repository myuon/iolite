struct Position {
  x: int,
  y: int,
}

struct Direction {
  x: int,
  y: int,
}

struct Ball {
  widget: Frame,
  pos: Position,
  dir: Direction,
}

module Ball {
  fun build(w: int, h: int): Ball {
    let widget = Frame::build(0, 0, w, h);
    widget.set_frame(FrameType::O_FLAT_FRAME());
    widget.set_color(255, 255, 255);

    let pos = Position { x: 0, y: 0 };
    let dir = Direction { x: 1, y: 1 };

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
  window.set_color(0, 0, 0);
  window.end();
  window.show();
  window.draw(fun () {
    Draw::set_draw_color(255, 255, 255);
    Draw::draw_rect(paddle_x, 540, 160, 20);

    return nil;
  });
  window.set_callback(fun (event: Event) {
    print_str("handle\n");
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
    ball.pos.x = ball.pos.x + 10 * ball.dir.x;
    ball.pos.y = ball.pos.y + 10 * ball.dir.y;

    if (ball.pos.y >= 540 - 40
        && (ball.pos.x > paddle_x - 40 && ball.pos.x < paddle_x + 160))
    {
      ball.dir.y = -1;
    }
    if (ball.pos.y <= 0) {
      ball.dir.y = 1;
    }
    if (ball.pos.x >= 800 - 40) {
      ball.dir.x = -1;
    }
    if (ball.pos.x <= 0) {
      ball.dir.x = 1;
    }
    if (ball.pos.y > 600) {
      ball.pos = Position { x: 0, y: 0 };
      ball.dir = Direction { x: 1, y: 1 };
    }

    ball.widget.resize(ball.pos.x, ball.pos.y, 40, 40);

    window.redraw();
    App::sleep(0.016);

    return nil;
  });
  app.run();
}
