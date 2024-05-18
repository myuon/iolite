fun call(f: () => nil) {
  return f();
}

let global = "global\n";

fun main() {
  let captured = "captured\n";

  call(fun () {
    let local = "local\n";
    print_str(global);
    print_str(captured);
    print_str(local);

    return nil;
  });
}
