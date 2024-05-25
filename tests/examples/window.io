struct Frame(rawptr);

module Frame {
  fun default(): Frame {
    return Frame(extcall_frame_default());
  }

  fun set_label(self, label: array[byte]) {
    return extcall_frame_set_label(self.!, label.ptr as rawptr, label.length);
  }
}

struct Button(rawptr);

module Button {
  fun default(title: array[byte]): Button {
    return Button(extcall_button_default(title.ptr as rawptr, title.length));
  }

  fun set_callback(self, callback: () => nil) {
    return extcall_button_set_callback(self.!, callback.ptr, callback.env);
  }
}

struct Flex(rawptr);

module Flex {
  fun default_fill(): Flex {
    return Flex(extcall_flex_default_fill());
  }

  fun column(self) {
    return extcall_flex_column(self.!);
  }

  fun set_margins(self, left: int, top: int, right: int, bottom: int) {
    return extcall_flex_set_margins(self.!, left, top, right, bottom);
  }

  fun end(self) {
    return extcall_flex_end(self.!);
  }
}

struct Window(rawptr);

module Window {
  fun build(x: int, y: int, width: int, height: int, title: array[byte]): Window {
    return Window(extcall_window_new(x, y, width, height, title.ptr as rawptr, title.length));
  }

  fun end(self) {
    return extcall_window_end(self.!);
  }

  fun show(self) {
    return extcall_window_show(self.!);
  }
}

struct App(rawptr);

module App {
  fun default(): App {
    return App(extcall_app_default());
  }

  fun wait(self): bool {
    return extcall_app_wait(self.!);
  }

  fun redraw(self) {
    return extcall_app_redraw(self.!);
  }
}

fun main() {
  let app = App::default();
  let count = 0;

  let window = Window::build(100, 200, 300, 400, "Hello, World!");

  let flex = Flex::default_fill();
  flex.column();
  flex.set_margins(30, 40, 30, 40);

  let button_inc = Button::default("+");

  let frame = Frame::default();
  frame.set_label(int_to_string(count));

  let button_dec = Button::default("-");

  flex.end();

  window.end();
  window.show();

  button_inc.set_callback(fun () {
    count = count + 1;
    frame.set_label(int_to_string(count));

    return nil;
  });

  button_dec.set_callback(fun () {
    if (count > 0) {
      count = count - 1;
      frame.set_label(int_to_string(count));
    }

    return nil;
  });

  while (app.wait()) {
    app.redraw();
  }

  return nil;
}
