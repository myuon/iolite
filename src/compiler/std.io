let heap_ptr = 0;

fun alloc(size: int) {
  let ptr = heap_ptr;
  heap_ptr = heap_ptr + size;
  return ptr;
}

// type array[T] {
//   ptr: ptr[T]
//   length: int,
// }
