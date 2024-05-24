struct Point {
    x: int,
    y: int,
}

module Point {
    fun build(x: int, y: int): Point {
        return Point { x: x, y: y };
    }

    fun sum(self): int {
        return p.x + p.y;
    }
}

fun main() {
    let p = Point::build(1, 2);

    println(p.sum());
}
