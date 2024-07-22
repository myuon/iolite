struct Wrapper(int);

module Wrapper {
  fun build(s: int): Wrapper {
    return Wrapper(s);
  }

  fun unwrap(w: Wrapper): int {
    return w.!;
  }
}

fun main() {
  let w = Wrapper::

  return nil;
}
