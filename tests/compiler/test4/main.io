fun call(f: () => nil) {
  return f();
}

fun main() {
  call(fun () {
    println("Hello, World!");
    return nil;
  });
}
