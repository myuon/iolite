fun call(f: () => nil) {
  return f();
}

fun main() {
  call(fun () {
    print_str("Hello, World!\n");
    return nil;
  });
}
