fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("SDL2", 500, 500);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let texture_creator = canvas.texture_creator();
  let surface = Surface::build(500, 500, 376840196);

  let r = 0;
  while (r < 4) {
    let g = 0;
    while (g < 25) {
      let b = 0;
      while (b < 25) {
        surface.fill_rect((r % 2) * 250 + g * 10, (r / 2) * 250 + b * 10, 10, 10, r * 64, g * 10, b * 10);
        b = b + 1;
      }

      g = g + 1;
    }

    r = r + 1;
  }

  while (true) {
    let time = SystemTime::now();
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    let texture = surface.as_texture(texture_creator);
    canvas.copy_texture_at(texture_creator, texture, 0, 0, 500, 500);

    let elapsed = SystemTime::duration_since(time);
    let elapsed_ms = elapsed.as_millis();
    let tick_ms = 16;
    if elapsed_ms < 16 {
      sleep(((16 - elapsed_ms) as float) / (1000 as float));
    } else {
      tick_ms = elapsed_ms;
    }

    window.set_title(concat_str("SDL2 - ", (1000 / tick_ms).to_string()));

    canvas.present();
  }
}
