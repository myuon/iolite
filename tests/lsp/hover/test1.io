struct Hoge {
  x: int,
}

fun f(x: int, y: byte) {
  return "foobar";
}

fun main() {
  let str = f(0, 48 as byte);
  let k = 128.to_string();

  let hoge = Hoge { x: 100 };
  let x = hoge.x;

  return str;
}
