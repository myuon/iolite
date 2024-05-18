// Requires posix-iol
declare fun extcall_write(fd: int, buf: rawptr, length: int): int;
// Requires libui-iol
// 10000
declare fun extcall_ui_init(): nil;
// 10001
declare fun extcall_ui_new_window(title: rawptr, width: int, height: int, has_menubar: int): rawptr;
// 10002
declare fun extcall_ui_control_show(window: rawptr): nil;
// 10003
declare fun extcall_ui_main(): nil;
// 10004
declare fun extcall_ui_draw_fill(context: rawptr, path: rawptr, brush: rawptr): nil;
// 10005
declare fun extcall_ui_struct_ui_draw_brush(type: int, r: int, g: int, b: int, a: int, x0: float, y0: float, x1: float, y1: float, outer_radius: float, stops: rawptr, num_stops: int): nil;
// 10006
declare fun extcall_ui_new_area(): nil;
// 10007
declare fun extcall_ui_new_area_handler(draw_ptr: rawptr, draw_env: rawptr): nil;

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
