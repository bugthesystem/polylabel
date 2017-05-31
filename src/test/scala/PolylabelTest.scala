import org.scalatest._


class PolylabelTest extends FlatSpec with Matchers {

  import polylabel._

  it should "works on degenerate polygons" in {
    // [[[0, 0], [1, 0], [2, 0], [0, 0]]]
    val polygon = Array(Array(Point(0, 0), Point(1, 0), Point(2, 0), Point(0, 0)))

    val p1 = polylabel(polygon)


    // [[[0, 0], [1, 0], [1, 1], [1, 0], [0, 0]]]
    val p2 = polylabel(Array[Array[Point]](Array[Point](Point(0, 0), Point(1, 0), Point(1, 1), Point(1, 0), Point(0, 0))))


    p1(0) should be (0)
    p1(1) should be (0)

    p2(0) should be (0)
    p2(1) should be (0)

  }
}
