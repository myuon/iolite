fun f(handler: (int) => int) {
  let wrapped = fun (n: int) {
    return handler(n + 1);
  };

  return wrapped(200);
}

fun main() {
  return f(fun (arg: int) {
    return arg * 2;
  });
}
