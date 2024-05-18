fun call(f: () => int): int {
  return f();
}

let global = 10;

fun main(): int {
  let captured = 20;

  return call(fun () {
    let local = 30;

    return captured * global - local;
  });
}
