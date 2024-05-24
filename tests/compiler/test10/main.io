struct Point {
    x: int,
    y: int,
}

module Point {
    fun build(x: int, y: int): Point {
        return Point { x: x, y: y };
    }

    fun sum(self): int {
        return self.x + self.y;
    }
}

fun main(): int {
    let p = Point::build(10, 20);

    return p.sum();
}
