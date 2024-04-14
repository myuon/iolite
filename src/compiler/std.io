let heap_ptr = 0 as ptr[byte];

fun alloc(size: int) {
  let ptr = heap_ptr;
  heap_ptr = heap_ptr.offset(size);
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
