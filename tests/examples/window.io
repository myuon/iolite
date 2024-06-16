fun main() {
  let sdl_context = SDL::init();
  let video = sdl_context.video();

  let window = video.window("SDL2", 800, 600);

  let canvas = window.get_canvas();

  canvas.set_draw_color(0, 255, 255);
  canvas.clear();
  canvas.present();

  let event_pump = sdl_context.event_pump();
  let i = 0;
  while (true) {
    let event  = event_pump.poll();
    if (event.is_quit()) {
      return nil;
    }

    i = (i + 1) % 255;
    canvas.set_draw_color(i, 64, 255 - i);
    canvas.clear();

    sleep(0.016);
    canvas.present();
  }
}
