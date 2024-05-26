fun inc(x: int): int {
  return x + 1;
}

fun double(x: int): int {
  return x * 2;
}

fun div3(x: int): int {
  return x / 3;
}

fun main() {
  return double(inc(div3(30)));
}
