fun main() {
  let handler = fun (n: int): int {
    return n * 2;
  };
  let handler2 = fun (n: int): int {
    return handler(n + 1);
  };
  let handler3 = fun (n: int): int {
    return handler2(n / 3);
  };

  return handler3(30);
}

