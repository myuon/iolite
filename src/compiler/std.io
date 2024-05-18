// Requires posix-lib
declare fun extcall_write(fd: int, buf: rawptr, length: int): int;
// Requires fltk-lib
declare fun extcall_window_new(x: int, y: int, width: int, height: int, title_ptr: rawptr, title_len: int): rawptr;
declare fun extcall_app_default(): rawptr;
declare fun extcall_app_run(app: rawptr);
declare fun extcall_button_default(title_ptr: rawptr, title_len: int): rawptr;
declare fun extcall_flex_default_fill(): rawptr;
declare fun extcall_flex_column(flex: rawptr): rawptr;
declare fun extcall_flex_set_margins(flex: rawptr, left: int, top: int, right: int, bottom: int);
declare fun extcall_flex_set_pad(flex: rawptr, pad: int);
declare fun extcall_flex_end(flex: rawptr);
declare fun extcall_window_end(window: rawptr);
declare fun extcall_window_show(window: rawptr);

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
