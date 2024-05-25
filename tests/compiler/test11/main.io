struct Point(int);

module Point {
    fun build(x: int): Point {
        return Point(x);
    }

    fun get(self): int {
        return self.!;
    }
}

fun main(): int {
    let p = Point::build(10);

    return p.get();
}
