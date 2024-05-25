fun main() {
  let app = App::default();
  let count = 0;

  let window = Window::build(100, 200, 300, 400, "Hello, World!");

  let flex = Flex::default_fill();
  flex.column();
  flex.set_margins(30, 40, 30, 40);

  let button_inc = Button::default("+");

  let frame = Frame::default();
  frame.set_label(count.to_string());

  let button_dec = Button::default("-");

  flex.end();

  window.end();
  window.show();

  button_inc.set_callback(fun () {
    count = count + 1;
    frame.set_label(count.to_string());

    return nil;
  });

  button_dec.set_callback(fun () {
    if (count > 0) {
      count = count - 1;
      frame.set_label(count.to_string());
    }

    return nil;
  });

  while (app.wait()) {
    app.redraw();
  }

  return nil;
}
