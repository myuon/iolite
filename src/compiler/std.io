let heap_ptr = 40;

fun alloc(size) {
  let ptr = heap_ptr;
  heap_ptr = heap_ptr + size;
  return ptr;
}
