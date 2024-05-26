fun main() {
  let callback = fun (x: int, y: int, z: int) {
    return x - y - z;
  };

  return callback(10, 5, 3);
}
