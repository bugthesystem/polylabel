import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


object polylabel {

  //get squared distance from a point to a segment
  def getSegDistSq(px: Double, py: Double, a: Point, b: Point): Double = {

    var x = a.x
    var y = a.y
    var dx = b.x - x
    var dy = b.y - y

    if (dx != 0 || dy != 0) {

      val t = ((px - x) * dx + (py - y) * dy) / (dx * dx + dy * dy)

      if (t > 1) {
        x = b.x
        y = b.y

      } else if (t > 0) {
        x += dx * t
        y += dy * t
      }
    }

    dx = px - x
    dy = py - y

    dx * dx + dy * dy
  }

  // get polygon centroid
  def getCentroidCell(polygon: Array[Array[Point]]): Cell = {
    var area = 0.0
    var x = 0.0
    var y = 0.0
    val points = polygon(0)

    var i = 0
    val len = points.length
    var j = len - 1

    while (i < len) {
      val a = points(i)
      val b = points(j)
      val f = a.x * b.y - b.x * a.y
      x += (a.x + b.x) * f
      y += (a.y + b.y) * f
      area += f * 3


      i += 1
      j = i
    }

    if (area == 0) Cell(points(0).x, points(0).y, 0, polygon) else Cell(x / area, y / area, 0, polygon)
  }

  case class Point(x: Double, y: Double)

  case class Cell(x: Double /* cell center x */ ,
                  y: Double /* cell center y */ ,
                  h: Double /* half the cell size */ ,
                  polygon: Array[Array[Point]]) {
    var d: Double = pointToPolygonDist(x, y, polygon) // distance from cell center to polygon
    var max: Double = this.d + this.h * Math.sqrt(2); // max distance to polygon within a cell

    // signed distance from point to polygon outline (negative if point is outside)
    private def pointToPolygonDist(x: Double, y: Double, polygon: Array[Array[Point]]): Double = {
      var inside = false
      var minDistSq: Double = Double.MaxValue

      for (k <- polygon.indices) {
        val ring = polygon(k)


        val len = ring.length
        var i = 0
        var j = len - 1

        while (i < len) {
          val a = ring(i)
          val b = ring(j)

          if (((a.y > y) != (b.y > y)) && (x < (b.x - a.x) * (y - a.y) / (b.y - a.y) + a.x)) inside = !inside

          minDistSq = Math.min(minDistSq, getSegDistSq(x, y, a, b))
          i += 1
          j = i
        }

      }

      val sqRes = Math.sqrt(minDistSq)
      if (inside) sqRes else -1 * sqRes
    }
  }

  def polylabel(polygon: Array[Array[Point]], precision: Double = 1.0, debug: Boolean = false): Array[Double] = {

    // find the bounding box of the outer ring
    var minX, minY, maxX, maxY = 0.0
    for (i <- polygon(0).indices) {
      val p = polygon(0)(i)
      if (i == 0 || p.x < minX) minX = p.x
      if (i == 0 || p.y < minY) minY = p.y
      if (i == 0 || p.x > maxX) maxX = p.x
      if (i == 0 || p.y > maxY) maxY = p.y
    }

    val width = maxX - minX
    val height = maxY - minY
    var cellSize = Math.min(width, height)
    var h = cellSize / 2

    var cellQueue = mutable.PriorityQueue.empty[Cell](Ordering.by[Cell, Double](_.max))

    if (cellSize == 0) return Array[Double](minX, minY)


    // cover polygon with initial cells
    var x = minX
    while (x < maxX) {
      var y = minY
      while (y < maxY) {
        cellQueue += Cell(x + h, y + h, h, polygon)
        y += cellSize
      }
      x += cellSize
    }

    // take centroid as the first best guess
    var bestCell = getCentroidCell(polygon)

    // special case for rectangular polygons
    val bboxCell = Cell(minX + width / 2, minY + height / 2, 0, polygon)
    if (bboxCell.d > bestCell.d) bestCell = bboxCell

    var numProbes = cellQueue.length

    while (cellQueue.nonEmpty) {
      breakable {
        // pick the most promising cell from the queue
        val cell = cellQueue.dequeue()

        // update the best cell if we found a better one
        if (cell.d > bestCell.d) {
          bestCell = cell
          if (debug) println("found best " + (Math.round(1e4 * cell.d) / 1e4).toString + " after " + numProbes + " probes")
        }

        // do not drill down further if there's no chance of a better solution
        if (cell.max - bestCell.d <= precision) break

        // split the cell into four cells
        h = cell.h / 2
        cellQueue += Cell(cell.x - h, cell.y - h, h, polygon)
        cellQueue += Cell(cell.x + h, cell.y - h, h, polygon)
        cellQueue += Cell(cell.x - h, cell.y + h, h, polygon)
        cellQueue += Cell(cell.x + h, cell.y + h, h, polygon)
        numProbes += 4
      }
    }

    if (debug) {
      println(String.format("num probes: %d" + numProbes))
      println(String.format("best distance:  %f" + bestCell.d))
    }

    Array[Double](bestCell.x, bestCell.y)
  }
}
