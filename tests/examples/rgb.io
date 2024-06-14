fun main() {
  let app = App::default();
  let window = Window::build(100, 100, 100, 100, "RGB");
  let frame = Frame::default();

  window.end();
  window.show();
  frame.draw(fun () {
    window.make_current();

    let g = 0;
    while (g < 10) {
      let b = 0;
      while (b < 10) {
        Draw::draw_box(FrameType::THIN_UP_BOX(), g * 10, b * 10, 10, 10, 255, g * 10, b * 10);
        b = b + 1;
      }

      g = g + 1;
    }

    return nil;
  });

  app.run();
}
