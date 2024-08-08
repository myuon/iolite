fun main() {
  let sdl_context = SDL::init();
  let ttf_context = TTFContext::init();
  let video = sdl_context.video();
  let window = video.window("SDL Text", 800, 600);

  let cache = FcFontCache::build();
  let count = extcall_fc_font_cache_list_count(cache.!);
  print_str("Installed fonts:\n");
  let list = cache.list();
  for i in 0..list.length {
    print_str(list.(i).name);
    print_str("\n");
  }

  let font = cache.load_font("Hiragino Sans W4", 16);
  let surface = ttf_context.render(font, "Hello, World! / こんにちは、世界！", 255, 255, 255);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let texture_creator = canvas.texture_creator();

  let menu_surfaces = new[array[Surface]](list.length);
  for i in 0..list.length {
    menu_surfaces.(i) = ttf_context.render(font, list.(i).name, 255, 255, 255);
  }

  let selected = 0;

  let prev_pressed_down = false;
  let prev_pressed_up = false;

  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    if !prev_pressed_down && event_pump.is_scancode_pressed(Scancode::DOWN()) {
      selected = (selected + 1) % list.length;
    } else if !prev_pressed_up && event_pump.is_scancode_pressed(Scancode::UP()) {
      selected = (selected + list.length - 1) % list.length;
    }

    prev_pressed_down = event_pump.is_scancode_pressed(Scancode::DOWN());
    prev_pressed_up = event_pump.is_scancode_pressed(Scancode::UP());

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    for i in 0..menu_surfaces.length {
      if i == selected {
        canvas.set_draw_color(255, 255, 255);
        canvas.fill_rect(0, i * 20, 350, 20);

        let surface = ttf_context.render(font, list.(i).name, 0, 0, 0);
        let texture = surface.as_texture(texture_creator);
        canvas.copy_texture_at(texture_creator, texture, 0, i * 20, surface.width(), surface.height());
      } else {
        let surface = menu_surfaces.(i);
        let texture = surface.as_texture(texture_creator);
        canvas.copy_texture_at(texture_creator, texture, 0, i * 20, surface.width(), surface.height());
      }
    }

    canvas.present();
  }

  return nil;
}
