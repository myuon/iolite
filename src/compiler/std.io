// Requires posix-lib
declare fun extcall_write(fd: int, buf: rawptr, length: int): int;
// Requires sdl2-lib
declare fun extcall_sdl_init(): rawptr;
declare fun extcall_sdl_context_video(context: rawptr): rawptr;
declare fun extcall_sdl_context_event_pump(context: rawptr): rawptr;
declare fun extcall_event_pump_poll(pump: rawptr): rawptr;
declare fun extcall_event_pump_is_scancode_pressed(pump: rawptr, scancode: int): bool;
declare fun extcall_event_pump_mouse_x(pump: rawptr): int;
declare fun extcall_event_pump_mouse_y(pump: rawptr): int;
declare fun extcall_event_pump_is_mouse_button_down(pump: rawptr, mouse_button: int): bool;
declare fun extcall_event_is_quit(event: rawptr): bool;
declare fun extcall_video_window(video: rawptr, title_ptr: rawptr, title_len: int, width: int, height: int): rawptr;
declare fun extcall_window_get_canvas(window: rawptr): rawptr;
declare fun extcall_window_set_title(window: rawptr, title_ptr: rawptr, title_len: int): rawptr;
declare fun extcall_canvas_set_draw_color(canvas: rawptr, r: int, g: int, b: int);
declare fun extcall_canvas_clear(canvas: rawptr);
declare fun extcall_canvas_present(canvas: rawptr);
declare fun extcall_canvas_fill_rect(canvas: rawptr, x: int, y: int, width: int, height: int);
declare fun extcall_canvas_texture_creator(canvas: rawptr): rawptr;
declare fun extcall_canvas_copy_texture_at(canvas: rawptr, texture_creator: rawptr, texture: rawptr, dst_x: int, dst_y: int, dst_width: int, dst_height: int): rawptr;
declare fun extcall_surface_new(width: int, height: int, format: int): rawptr;
declare fun extcall_surface_as_texture(surface: rawptr, texture_creator: rawptr): rawptr;
declare fun extcall_surface_blit_to_canvas_at(surface: rawptr, canvas: rawptr, dst_x: int, dst_y: int);
declare fun extcall_surface_fill_rect(surface: rawptr, x: int, y: int, width: int, height: int, r: int, g: int, b: int);
declare fun extcall_surface_width(surface: rawptr): int;
declare fun extcall_surface_height(surface: rawptr): int;
declare fun extcall_sleep(sec: float);
declare fun extcall_time_now(): rawptr;
declare fun extcall_time_duration_since(time: rawptr): rawptr;
declare fun extcall_duration_as_millis(time: rawptr): int;
declare fun extcall_sdl_image_init(): rawptr;
declare fun extcall_sdl_image_load(path: rawptr, length: int): rawptr;
declare fun extcall_sdl_ttf_init(): rawptr;
declare fun extcall_sdl_ttf_load_font(path_ptr: rawptr, path_len: int, size: int): rawptr;
declare fun extcall_sdl_font_render_solid(font: rawptr, text_ptr: rawptr, text_len: int, r: int, g: int, b: int): rawptr;
declare fun extcall_sdl_font_render_blended(font: rawptr, text_ptr: rawptr, text_len: int, r: int, g: int, b: int): rawptr;
declare fun extcall_fc_font_cache_build(): rawptr;
declare fun extcall_fc_font_cache_load_font(cache: rawptr, name_ptr: rawptr, name_len: int, size: int): rawptr;

struct Duration(rawptr);

module Duration {
  fun as_millis(self): int {
    return extcall_duration_as_millis(self.!);
  }
}

struct SystemTime(rawptr);

module SystemTime {
  fun now(): SystemTime {
    return SystemTime(extcall_time_now());
  }

  fun duration_since(self): Duration {
    return Duration(extcall_time_duration_since(self.!));
  }
}

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

@[builtin_method(int, "abs")]
fun int_abs(x: int) {
  if (x < 0) {
    return -x;
  } else {
    return x;
  }
}

@[builtin_method(float, "abs")]
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

@[builtin_method(int, "to_string")]
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

@[builtin_method(array[byte], "concat")]
fun concat_str(a: array[byte], b: array[byte]): array[byte] {
  let text = new[array[byte]](a.length + b.length);
  let i = 0;
  while (i < a.length) {
    text.(i) = a.(i);
    i = i + 1;
  }
  let j = 0;
  while (j < b.length) {
    text.(i) = b.(j);
    i = i + 1;
    j = j + 1;
  }
  return text;
}

@[builtin_method(array[byte], "eq")]
fun eq_str(a: array[byte], b: array[byte]): bool {
  if (a.length != b.length) {
    return false;
  }

  for i in 0..a.length {
    if (a.(i) as int != b.(i) as int) {
      return false;
    }
  }

  return true;
}

fun assert(b: bool) {
  if (!b) {
    panic("assertion failed");
  }

  return nil;
}

@[test]
fun test_concat_str() {
  assert(eq_str(concat_str("abc", "abc"), "abcabc"));
  assert(eq_str(concat_str("", "abc"), "abc"));
  assert(eq_str(concat_str("abc", ""), "abc"));
  assert(eq_str(concat_str("", ""), ""));
}

@[test]
fun test_eq_str() {
  assert(eq_str("abc", "abc"));
  assert(!eq_str("abc", "def"));
  assert(!eq_str("abc", "abcd"));
  assert(!eq_str("abc", "ab"));
}

// SDL2 bindings

struct Scancode(int);

module Scancode {
  fun ESCAPE(): Scancode {
    return Scancode(41);
  }

  fun RIGHT(): Scancode {
    return Scancode(79);
  }

  fun LEFT(): Scancode {
    return Scancode(80);
  }

  fun DOWN(): Scancode {
    return Scancode(81);
  }

  fun UP(): Scancode {
    return Scancode(82);
  }
}

struct Event(rawptr);

module Event {
  fun is_quit(self): bool {
    return extcall_event_is_quit(self.!);
  }
}

struct MouseButton(int);

module MouseButton {
  fun LEFT(): MouseButton {
    return MouseButton(1);
  }

  fun RIGHT(): MouseButton {
    return MouseButton(3);
  }
}

struct MousePosition {
  x: int,
  y: int,
}

struct EventPump(rawptr);

module EventPump {
  fun poll(self): Event {
    return Event(extcall_event_pump_poll(self.!));
  }

  fun is_scancode_pressed(self, scancode: Scancode): bool {
    return extcall_event_pump_is_scancode_pressed(self.!, scancode.!);
  }

  fun mouse_position(self): MousePosition {
    return MousePosition {
      x: extcall_event_pump_mouse_x(self.!),
      y: extcall_event_pump_mouse_y(self.!),
    };
  }

  fun is_mouse_button_down(self, mouse_button: MouseButton): bool {
    return extcall_event_pump_is_mouse_button_down(self.!, mouse_button.!);
  }
}

struct Texture(rawptr);

struct TextureCreator(rawptr);

struct Canvas(rawptr);

module Canvas {
  fun set_draw_color(self, r: int, g: int, b: int) {
    return extcall_canvas_set_draw_color(self.!, r, g, b);
  }

  fun clear(self) {
    return extcall_canvas_clear(self.!);
  }

  fun present(self) {
    return extcall_canvas_present(self.!);
  }

  fun fill_rect(self, x: int, y: int, width: int, height: int) {
    return extcall_canvas_fill_rect(self.!, x, y, width, height);
  }

  fun texture_creator(self): TextureCreator {
    return TextureCreator(extcall_canvas_texture_creator(self.!));
  }

  fun copy_texture_at(self, texture_creator: TextureCreator, texture: Texture, dst_x: int, dst_y: int, dst_width: int, dst_height: int) {
    return extcall_canvas_copy_texture_at(self.!, texture_creator.!, texture.!, dst_x, dst_y, dst_width, dst_height);
  }
}

struct Window(rawptr);

module Window {
  fun get_canvas(self): Canvas {
    return Canvas(extcall_window_get_canvas(self.!));
  }

  fun set_title(self, title: array[byte]) {
    return extcall_window_set_title(self.!, title.ptr as rawptr, title.length);
  }
}

struct Surface(rawptr);

module Surface {
  fun build(width: int, height: int, pixel_format: int): Surface {
    return Surface(extcall_surface_new(width, height, pixel_format));
  }

  fun as_texture(self, texture_creator: TextureCreator): Texture {
    return Texture(extcall_surface_as_texture(self.!, texture_creator.!));
  }

  fun fill_rect(self, x: int, y: int, width: int, height: int, r: int, g: int, b: int) {
    return extcall_surface_fill_rect(self.!, x, y, width, height, r, g, b);
  }

  fun width(self): int {
    return extcall_surface_width(self.!);
  }

  fun height(self): int {
    return extcall_surface_height(self.!);
  }
}

struct VideoSubsystem(rawptr);

module VideoSubsystem {
  fun window(self, title: array[byte], width: int, height: int): Window {
    return Window(extcall_video_window(self.!, title.ptr as rawptr, title.length, width, height));
  }
}

struct SDL(rawptr);

module SDL {
  fun init(): SDL {
    return SDL(extcall_sdl_init());
  }

  fun video(self): VideoSubsystem {
    return VideoSubsystem(extcall_sdl_context_video(self.!));
  }

  fun event_pump(self): EventPump {
    return EventPump(extcall_sdl_context_event_pump(self.!));
  }
}

struct SDLImage(rawptr);

module SDLImage {
  fun init(): SDLImage {
    return SDLImage(extcall_sdl_image_init());
  }

  fun from_file(path: array[byte]): Surface {
    return Surface(extcall_sdl_image_load(path.ptr as rawptr, path.length));
  }
}

fun sleep(sec: float) {
  return extcall_sleep(sec);
}

struct Font(rawptr);

struct FcFontCache(rawptr);

module FcFontCache {
  fun build(): FcFontCache {
    return FcFontCache(extcall_fc_font_cache_build());
  }

  // When cstr is supported, change this to extcall_fc_font_cache_query(cstr): cstr
  fun load_font(self, name: array[byte], size: int): Font {
    return Font(extcall_fc_font_cache_load_font(self.!, name.ptr as rawptr, name.length, size));
  }
}

struct TTFContext(rawptr);

module TTFContext {
  fun init(): TTFContext {
    return TTFContext(extcall_sdl_ttf_init());
  }

  fun load_font(self, path: array[byte], size: int): Font {
    return Font(extcall_sdl_ttf_load_font(path.ptr as rawptr, path.length, size));
  }

  fun render_solid(self, font: Font, text: array[byte], r: int, g: int, b: int): Surface {
    return Surface(extcall_sdl_font_render_solid(font.!, text.ptr as rawptr, text.length, r, g, b));
  }

  fun render(self, font: Font, text: array[byte], r: int, g: int, b: int): Surface {
    return Surface(extcall_sdl_font_render_blended(font.!, text.ptr as rawptr, text.length, r, g, b));
  }
}
