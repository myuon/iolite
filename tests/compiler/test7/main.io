fun call(f: () => nil) {
  return f();
}

let global = "global";

fun main() {
  let captured = "captured";

  call(fun () {
    let local = "local";
    print_str(global);
    print_str(captured);
    print_str(local);

    return nil;
  });
}
