fun main() {
  let sdl_context = SDL::init();
  SDLImage::init();
  let video = sdl_context.video();
  let window = video.window("SDL Image", 500, 500);

  let canvas = window.get_canvas();

  let event_pump = sdl_context.event_pump();

  let texture_creator = canvas.texture_creator();
  let surface = SDLImage::from_file("tests/examples/assets/rgb.png");

  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    canvas.set_draw_color(0, 0, 0);
    canvas.clear();

    let texture = surface.as_texture(texture_creator);
    canvas.copy_texture_at(texture_creator, texture, 0, 0, 500, 500);

    canvas.present();
  }

  return nil;
}
