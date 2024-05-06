fun main() {
  let text = "Hello, World!\n";
  extcall_write(1, text.ptr as rawptr, text.length);
}
