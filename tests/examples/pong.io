struct Position {
  x: int,
  y: int,
}

struct Direction {
  x: int,
  y: int,
}

struct Ball {
  pos: Position,
  dir: Direction,
}

module Ball {
  fun build(w: int, h: int): Ball {
    let pos = Position { x: 0, y: 0 };
    let dir = Direction { x: 1, y: 1 };

    return Ball {
      pos: pos,
      dir: dir,
    };
  }
}

fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("Pong!", 800, 600);

  let ball = Ball::build(40, 40);

  let paddle_x = 320;

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();
  while (true) {
    let time = SystemTime::now();
    let event = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }
    if (event_pump.is_scancode_pressed(Scancode::ESCAPE())) {
      return nil;
    }

    let mouse = event_pump.mouse_position();
    paddle_x = mouse.x - 80;

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

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    canvas.set_draw_color(255, 255, 255);
    canvas.fill_rect(paddle_x, 540, 160, 20);
    canvas.fill_rect(ball.pos.x, ball.pos.y, 40, 40);

    let elapsed = SystemTime::duration_since(time);
    let elapsed_ms = elapsed.as_millis();

    if elapsed_ms <= 16 {
      sleep((16.67 - elapsed_ms as float) / (1000 as float));
      window.set_title(concat_str("Pong! - ", 60.to_string()));
    } else {
      let fps = 1000 / elapsed_ms;
      window.set_title(concat_str("Pong! - ", fps.to_string()));
    }

    canvas.present();
  }
}
