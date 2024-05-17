fun call(f: () => nil) {
  return f();
}

fun main() {
  call(fun () {
    let text = "Hello, World!\n";
    print_str(text);

    return nil;
  });
}
