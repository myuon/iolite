// Requires posix-lib
declare fun extcall_write(fd: int, buf: rawptr, length: int): int;
// Requires fltk-lib
declare fun extcall_window_new(x: int, y: int, width: int, height: int, title_ptr: rawptr, title_len: int): rawptr;
declare fun extcall_app_default(): rawptr;
declare fun extcall_app_run(app: rawptr);
declare fun extcall_app_wait(app: rawptr): bool;
declare fun extcall_app_redraw(app: rawptr);
declare fun extcall_app_event_key(app: rawptr): int;
declare fun extcall_app_event_coords(app: rawptr): int;
declare fun extcall_app_quit(app: rawptr);
declare fun extcall_app_sleep(sec: float);
declare fun extcall_app_add_idle(app: rawptr, handler_ptr: rawptr, handler_env: rawptr);
declare fun extcall_button_default(title_ptr: rawptr, title_len: int): rawptr;
declare fun extcall_button_set_callback(button: rawptr, callback_ptr: rawptr, callback_env: rawptr);
declare fun extcall_frame_default(): rawptr;
declare fun extcall_frame_set_rectangle(frame: rawptr, x: int, y: int, width: int, height: int);
declare fun extcall_frame_set_label(frame: rawptr, title_ptr: rawptr, title_len: int);
declare fun extcall_frame_resize(frame: rawptr, x: int, y: int, width: int, height: int);
declare fun extcall_frame_set_color(frame: rawptr, r: int, g: int, b: int);
declare fun extcall_frame_set_frame(frame: rawptr, frame_type: int);
declare fun extcall_flex_default_fill(): rawptr;
declare fun extcall_flex_column(flex: rawptr): rawptr;
declare fun extcall_flex_set_margins(flex: rawptr, left: int, top: int, right: int, bottom: int);
declare fun extcall_flex_set_pad(flex: rawptr, pad: int);
declare fun extcall_flex_end(flex: rawptr);
declare fun extcall_window_end(window: rawptr);
declare fun extcall_window_show(window: rawptr);
declare fun extcall_window_set_handler(window: rawptr, handler_ptr: rawptr, handler_env: rawptr);
declare fun extcall_window_draw(window: rawptr, handler_ptr: rawptr, handler_env: rawptr);
declare fun extcall_window_redraw(window: rawptr);
declare fun extcall_draw_set_draw_color(r: int, g: int, b: int);
declare fun extcall_draw_draw_rect(x: int, y: int, width: int, height: int);

let heap_ptr = 0 as ptr[byte];

fun align_int(x: int, alignment: int) {
  return (x + alignment - 1) / alignment * alignment;
}

fun alloc(size: int) {
  let aligned_size = align_int(size, 8);
  let ptr = heap_ptr;
  heap_ptr = heap_ptr.offset(aligned_size);
  return ptr;
}

// type array[T] {
//   ptr: ptr[T]
//   length: int,
// }

// fun new_array[T](length: int) {
//   let ptr = alloc(length * sizeof[T]());
//   return array[T] { ptr: ptr, length: length };
// }

fun int_abs(x: int) {
  if (x < 0) {
    return -x;
  } else {
    return x;
  }
}

fun float_abs(x: float) {
  if (x < 0.0) {
    return -x;
  } else {
    return x;
  }
}

fun ptr_offset(p: ptr[byte], d: int) {
  return (p as int + d) as ptr[byte];
}

let fd_stdout = 1;

fun print_str(text: array[byte]) {
  extcall_write(fd_stdout, text.ptr as rawptr, text.length);

  return nil;
}

fun to_cstr(text: array[byte]): ptr[byte] {
  let cstr = new[ptr[byte]](text.length + 1);

  let i = 0;
  while (i < text.length) {
    cstr.(i) = text.(i);
    i = i + 1;
  }

  cstr.(text.length) = 0 as byte;

  return cstr;
}

fun panic(text: array[byte]) {
  print_str(text);
  abort();
}

fun int_to_string(n: int): array[byte] {
  if (n < 0) {
    panic("int_to_string: negative number");
  }

  let digit = 1;
  let m = int_abs(n);
  while (m >= 10) {
    m = m / 10;
    digit = digit + 1;
  }

  let text = new[array[byte]](digit);
  let i = digit - 1;
  let n = int_abs(n);
  while (i >= 0) {
    text.(i) = (n % 10 + 48) as byte;
    n = n / 10;
    i = i - 1;
  }

  return text;
}

// fltk

struct Event(int);

module Event {
  fun from_bits(bits: int): Event {
    return Event(bits);
  }

  fun NO_EVENT(): Event {
    return Event(0);
  }

  fun PUSH(): Event {
    return Event(1);
  }

  fun RELEASED(): Event {
    return Event(2);
  }

  fun ENTER(): Event {
    return Event(3);
  }

  fun LEAVE(): Event {
    return Event(4);
  }

  fun DRAG(): Event {
    return Event(5);
  }

  fun FOCUS(): Event {
    return Event(6);
  }

  fun UNFOCUS(): Event {
    return Event(7);
  }

  fun KEYDOWN(): Event {
    return Event(8);
  }

  fun KEYUP(): Event {
    return Event(9);
  }

  fun CLOSE(): Event {
    return Event(10);
  }

  fun MOVE(): Event {
    return Event(11);
  }

  fun SHORTCUT(): Event {
    return Event(12);
  }

  fun DEACTIVATE(): Event {
    return Event(13);
  }

  fun ACTIVATE(): Event {
    return Event(14);
  }

  fun HIDE(): Event {
    return Event(15);
  }

  fun SHOW(): Event {
    return Event(16);
  }

  fun PASTE(): Event {
    return Event(17);
  }

  fun SELECTION_CLEAR(): Event {
    return Event(18);
  }

  fun MOUSE_WHEEL(): Event {
    return Event(19);
  }

  fun DND_ENTER(): Event {
    return Event(20);
  }

  fun DND_DROP(): Event {
    return Event(21);
  }

  fun DND_LEAVE(): Event {
    return Event(22);
  }

  fun DND_RELEASE(): Event {
    return Event(23);
  }

  fun SCREEN_CONFIG_CHANGED(): Event {
    return Event(24);
  }

  fun FULLSCREEN(): Event {
    return Event(25);
  }

  fun ZOOM_GESTURE(): Event {
    return Event(26);
  }

  fun ZOOM(): Event {
    return Event(27);
  }

  fun RESIZE(): Event {
    return Event(28);
  }
}

struct Key(int);

module Key {
  fun from_bits(bits: int): Key {
    return Key(bits);
  }

  fun ESCAPE(): Key {
    return Key(65307);
  }
}

struct FrameType(int);

module FrameType {
  fun O_FLAT_FRAME(): FrameType {
    return FrameType(29);
  }

  fun to_int(self): int {
    return self.!;
  }
}

struct Frame(rawptr);

module Frame {
  fun default(): Frame {
    return Frame(extcall_frame_default());
  }

  fun set_label(self, label: array[byte]) {
    return extcall_frame_set_label(self.!, label.ptr as rawptr, label.length);
  }

  fun set_rectangle(self, x: int, y: int, w: int, h: int) {
    return extcall_frame_set_rectangle(self.!, x, y, w, h);
  }

  fun resize(self, x: int, y: int, w: int, h: int) {
    return extcall_frame_resize(self.!, x, y, w, h);
  }

  fun set_color(self, r: int, g: int, b: int) {
    return extcall_frame_set_color(self.!, r, g, b);
  }

  fun set_frame(self, frame_type: FrameType) {
    return extcall_frame_set_frame(self.!, frame_type.to_int());
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

  fun set_callback(self, handler_: (Event) => nil) {
    let handler = fun (e: int) {
      handler_(Event::from_bits(e));
    };

    return extcall_window_set_handler(self.!, handler.ptr, handler.env);
  }

  fun draw(self, handler: () => nil) {
    return extcall_window_draw(self.!, handler.ptr, handler.env);
  }

  fun redraw(self) {
    return extcall_window_redraw(self.!);
  }
}

struct Point {
  x: int,
  y: int,
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

  fun run(self) {
    while (self.wait()) {
      self.redraw();
    }
  }

  fun event_key(self): Key {
    return Key::from_bits(extcall_app_event_key(self.!));
  }

  fun event_coords(self): Point {
    let value = extcall_app_event_coords(self.!);

    return Point {
      x: value % 65535, // value & 0xffff
      y: value / 65536, // value >> 16
    };
  }

  fun quit(self) {
    return extcall_app_quit(self.!);
  }

  fun add_idle(self, handler: () => nil) {
    return extcall_app_add_idle(self.!, handler.ptr, handler.env);
  }

  fun sleep(sec: float) {
    return extcall_app_sleep(sec);
  }
}

struct Draw(nil);

module Draw {
  fun set_draw_color(r: int, g: int, b: int) {
    return extcall_draw_set_draw_color(r, g, b);
  }

  fun draw_rect(x: int, y: int, w: int, h: int) {
    return extcall_draw_draw_rect(x, y, w, h);
  }
}
