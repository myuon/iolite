fun main() {
  let sdl_context = SDL::init();
  let ttf_context = TTFContext::init();
  let video = sdl_context.video();
  let window = video.window("SDL Text", 500, 500);

  let cache = FcFontCache::build();
  let count = extcall_fc_font_cache_list_count(cache.!);
  print_str("Installed fonts:\n");
  let list = cache.list();
  for i in 0..list.length {
    print_str(list.(i).name);
    print_str("\n");
  }

  let font = cache.load_font("Noto Sans CJK JP Regular", 24);
  let surface = ttf_context.render(font, "Hello, World! / こんにちは、世界！", 255, 255, 255);

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
