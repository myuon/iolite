fun f() {
  return "f";
}

fun g() {
  return f();
}

fun main() {
  let r = g();

  return r;
}
