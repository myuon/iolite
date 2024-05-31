fun f(n: int) {
  let wrapped = fun (m: int) {
    return n - m;
  };

  return wrapped(50);
}

fun main() {
  return f(200);
}
