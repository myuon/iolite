struct Point {
  x: int,
}

module Point {
  fun get_x(self): int {
    return self.x;
  }
}

fun main() {
  let point = Point { x: 10 };
  let x = point.();

  return nil;
}
