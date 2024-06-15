fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("SDL2", 800, 600);

  let canvas = window.get_canvas();

  canvas.set_draw_color(255, 0, 0);
  canvas.clear();
  canvas.present();

  let event_pump = sdl_context.event_pump();
  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    sleep(0.016);
    canvas.clear();
    canvas.present();
  }
}
