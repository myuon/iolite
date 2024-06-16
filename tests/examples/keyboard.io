fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("key", 800, 600);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let x = 400;
  let y = 300;
  let v = 10;
  while (true) {
    let event = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }
    if (event_pump.is_scancode_pressed(Scancode::ESCAPE())) {
      return nil;
    }

    if (event_pump.is_scancode_pressed(Scancode::RIGHT())) {
      x = x + v;
    } else if (event_pump.is_scancode_pressed(Scancode::LEFT())) {
      x = x - v;
    } else if (event_pump.is_scancode_pressed(Scancode::UP())) {
      y = y - v;
    } else if (event_pump.is_scancode_pressed(Scancode::DOWN())) {
      y = y + v;
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    canvas.set_draw_color(255, 255, 255);
    canvas.fill_rect(x, y, 100, 100);

    sleep(0.016);
    canvas.present();
  }
}
