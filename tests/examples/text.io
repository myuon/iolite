fun main() {
  let sdl_context = SDL::init();
  let ttf_context = TTFContext::init();
  let video = sdl_context.video();
  let window = video.window("SDL Text", 500, 500);

  let cache = FcFontCache::build();

  let font = cache.load_font("Noto Sans Regular", 24);
  let surface = ttf_context.render(font, "Hello, World!", 255, 255, 255);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let texture_creator = canvas.texture_creator();

  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    let texture = surface.as_texture(texture_creator);
    canvas.copy_texture_at(texture_creator, texture, 0, 0, surface.width(), surface.height());

    canvas.present();
  }

  return nil;
}
