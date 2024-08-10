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

  let pressing_down = 0;
  let pressing_up = 0;

  let container_width = 350;
  let container_height = list.length * 20;
  let scroll_position_y = 0;

  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    if event_pump.is_scancode_pressed(Scancode::DOWN()) {
      pressing_down = pressing_down + 1;
    } else {
      pressing_down = 0;
    }
    if event_pump.is_scancode_pressed(Scancode::UP()) {
      pressing_up = pressing_up + 1;
    } else {
      pressing_up = 0;
    }
    
    if pressing_down == 1 || pressing_down == 5 || (pressing_down > 10 && pressing_down % 2 == 0) {
      selected = (selected + 1) % list.length;
    }
    if pressing_up == 1 || pressing_up == 5 || (pressing_up > 10 && pressing_up % 2 == 0) {
      if (selected > 0) {
        selected = selected - 1;
      }
    }

    if (selected - scroll_position_y + 1) * 20 > 600 {
      scroll_position_y = scroll_position_y + 1;
    }
    if (selected - scroll_position_y) * 20 < 0 {
      scroll_position_y = scroll_position_y - 1;
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    for i in 0..(600 / 20) {
      if (scroll_position_y + i) == selected {
        canvas.set_draw_color(255, 255, 255);
        canvas.fill_rect(0, i * 20, container_width, 20);

        let surface = ttf_context.render(font, list.(scroll_position_y + i).name, 0, 0, 0);
        let texture = surface.as_texture(texture_creator);
        canvas.copy_texture_at(texture_creator, texture, 0, i * 20, surface.width(), surface.height());
      } else {
        let surface = menu_surfaces.(scroll_position_y + i);
        let texture = surface.as_texture(texture_creator);
        canvas.copy_texture_at(texture_creator, texture, 0, i * 20, surface.width(), surface.height());
      }
    }

    canvas.set_draw_color(255, 255, 255);
    canvas.fill_rect(container_width, 600 * scroll_position_y / list.length, 10, 600 * 600 / container_height);

    canvas.present();
  }

  return nil;
}
