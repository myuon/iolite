fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("Breakout", 500, 500);

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
    paddle_x = mouse.x - 50;

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    canvas.set_draw_color(255, 255, 255);
    canvas.fill_rect(paddle_x, 500 - 100, 100, 10);

    canvas.present();
  }

  return nil;
}
