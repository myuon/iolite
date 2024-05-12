import lib;

fun main() {
  f();

  let name = "main.io\n";
  extcall_write(1, name.ptr as rawptr, name.length);
}
