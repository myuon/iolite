fun call(f: () => nil) {
  return f();
}

fun main() {
  call(fun () {
    let text = "Hello, World!\n";
    extcall_write(1, text.ptr as rawptr, text.length);

    return nil;
  });
}
