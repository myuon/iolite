struct Position {
  x: int,
  y: int,
}

fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let screen_width = 500;
  let screen_height = 500;

  let window = video.window("Breakout", screen_width, screen_height);

  let paddle_x = 320;
  let ball_position = Position { x: 250, y: 250 };
  let ball_velocity = Position { x: 1, y: 1 };

  let gamestate = "start";

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
    paddle_x = mouse.x - 50;
    if (eq_str(gamestate, "start")) {
      ball_position.x = mouse.x;
      ball_position.y = screen_height - 130;
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    canvas.set_draw_color(255, 255, 255);
    canvas.fill_rect(paddle_x, screen_width - 80, 100, 10);

    canvas.set_draw_color(255, 255, 0);
    canvas.fill_rect(ball_position.x, ball_position.y, 10, 10);

    let margin_h = 20;
    let margin_v = 20;
    let gap = 5;

    let block_area_width = screen_width - 2 * margin_h;
    let block_area_height = 150 - 2 * margin_v;

    let blocks_vertical = 5;
    let blocks_horizontal = 5;

    let block_width = (block_area_width - (blocks_horizontal - 1) * gap) / blocks_horizontal;
    let block_height = (block_area_height - (blocks_vertical - 1) * gap) / blocks_vertical;

    for i in 0..blocks_vertical {
      for j in 0..blocks_horizontal {
        if i == 0 {
          canvas.set_draw_color(252, 234, 187);
        } else if i == 1 {
          canvas.set_draw_color(180, 211, 166);
        } else if i == 2 {
          canvas.set_draw_color(109, 184, 166);
        } else if i == 3 {
          canvas.set_draw_color(51, 151, 169);
        } else {
          canvas.set_draw_color(54, 114, 158);
        }

        canvas.fill_rect(
          margin_h + j * (block_width + gap),
          margin_v + i * (block_height + gap),
          block_width,
          block_height,
        );
      }
    }

    canvas.present();
  }

  return nil;
}
