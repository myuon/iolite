struct Position {
  x: int,
  y: int,
}

struct Rectangle {
  x: int,
  y: int,
  width: int,
  height: int,
}

module Rectangle {
  fun contains(self, position: Position): bool {
    return (
      position.x >= self.x &&
      position.x <= self.x + self.width &&
      position.y >= self.y &&
      position.y <= self.y + self.height
    );
  }
}

struct Color {
  r: int,
  g: int,
  b: int,
}

struct Block {
  rect: Rectangle,
  color: Color,
  is_visible: bool,
}

let title = "Breakout";

fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let screen_width = 500;
  let screen_height = 500;

  let window = video.window(title, screen_width, screen_height);

  let paddle_rect = Rectangle { x: 320, y: screen_height - 80, width: 100, height: 10 };

  let ball_position = Position { x: 250, y: 250 };
  let ball_velocity = Position { x: 6, y: -6 };

  let margin_h = 20;
  let margin_v = 20;
  let gap = 5;

  let block_area_width = screen_width - 2 * margin_h;
  let block_area_height = 150 - 2 * margin_v;

  let blocks_vertical = 5;
  let blocks_horizontal = 5;

  let block_width = (block_area_width - (blocks_horizontal - 1) * gap) / blocks_horizontal;
  let block_height = (block_area_height - (blocks_vertical - 1) * gap) / blocks_vertical;

  let blocks = new[array[Block]](blocks_horizontal * blocks_vertical);
  for i in 0..blocks_vertical {
    for j in 0..blocks_horizontal {
      let index = i * blocks_horizontal + j;

      let color = Color { r: 0, g: 0, b: 0 };
      if i == 0 {
        color = Color { r: 252, g: 234, b: 187 };
      } else if i == 1 {
        color = Color { r: 180, g: 211, b: 166 };
      } else if i == 2 {
        color = Color { r: 109, g: 184, b: 166 };
      } else if i == 3 {
        color = Color { r: 51, g: 151, b: 169 };
      } else {
        color = Color { r: 54, g: 114, b: 158 };
      }

      blocks.(index) = Block {
        rect: Rectangle {
          x: margin_h + j * (block_width + gap),
          y: margin_v + i * (block_height + gap),
          width: block_width,
          height: block_height,
        },
        color: color,
        is_visible: true,
      };
    }
  }

  let gamestate = "start";

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let count = 0;
  let sleep_timer = 16.67 / (1000 as float);
  while (true) {
    count = count + 1;
    let time = SystemTime::now();
    let event = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }
    if (event_pump.is_scancode_pressed(Scancode::ESCAPE())) {
      return nil;
    }

    if (event_pump.is_mouse_button_down(MouseButton::LEFT())) {
      if (eq_str(gamestate, "start")) {
        gamestate = "play";
      }
    }

    let mouse = event_pump.mouse_position();
    paddle_rect.x = mouse.x - 50;
    if (eq_str(gamestate, "start")) {
      ball_position.x = mouse.x;
      ball_position.y = screen_height - 130;
    }
    if (eq_str(gamestate, "play")) {
      ball_position.x = ball_position.x + ball_velocity.x;
      ball_position.y = ball_position.y + ball_velocity.y;

      if (ball_position.x < 0 || ball_position.x > screen_width - 10) {
        ball_velocity.x = -ball_velocity.x;
      }
      if (ball_position.y < 0) {
        ball_velocity.y = -ball_velocity.y;
      }
      if (ball_position.y > screen_height - 10) {
        gamestate = "start";
      }
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    canvas.set_draw_color(255, 255, 255);
    canvas.fill_rect(paddle_rect.x, paddle_rect.y, paddle_rect.width, paddle_rect.height);

    canvas.set_draw_color(255, 255, 0);
    canvas.fill_rect(ball_position.x, ball_position.y, 10, 10);

    for block in blocks {
      if (block.is_visible) {
        canvas.set_draw_color(block.color.r, block.color.g, block.color.b);
        canvas.fill_rect(block.rect.x, block.rect.y, block.rect.width, block.rect.height);
      }

      if (block.is_visible && block.rect.contains(ball_position)) {
        block.is_visible = false;
        ball_velocity.y = -ball_velocity.y;
      }
    }

    if (paddle_rect.contains(ball_position)) {
      ball_velocity.y = -ball_velocity.y;
    }

    if (count % 5 == 0) {
      let elapsed = SystemTime::duration_since(time);
      let elapsed_ms = elapsed.as_millis();

      if elapsed_ms <= 16 {
        sleep_timer = (16.67 - elapsed_ms as float) / (1000 as float);
        window.set_title(concat_str(concat_str(title, " - "), 60.to_string()));
      } else {
        let fps = 1000 / elapsed_ms;
        window.set_title(concat_str(concat_str(title, " - "), fps.to_string()));
      }
    }

    sleep(sleep_timer);

    canvas.present();
  }

  return nil;
}
