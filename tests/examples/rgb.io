fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("SDL2", 500, 500);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();
  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    canvas.clear();

    let r = 0;
    while (r < 4) {
      let g = 0;
      while (g < 25) {
        let b = 0;
        while (b < 25) {
          canvas.set_draw_color(r * 64, g * 10, b * 10);
          canvas.fill_rect((r % 2) * 250 + g * 10, (r / 2) * 250 + b * 10, 10, 10);
          b = b + 1;
        }

        g = g + 1;
      }

      r = r + 1;
    }

    sleep(0.016);
    canvas.present();
  }
}
