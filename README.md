# polylabel.scala
A fast algorithm for finding the pole of inaccessibility of a polygon

![](https://raw.githubusercontent.com/ziyasal/polylabel/master/misc/img.png)

## Sample Code

```scala

import polylabel._

// [[[0, 0], [1, 0], [2, 0], [0, 0]]]
val polygon = Array(Array(Point(0, 0), Point(1, 0), Point(2, 0), Point(0, 0)))

val p1 = polylabel(polygon)


//p1: Array[Double] = Array(0.0, 0.0)

 ```

## TODO
 - Documentation
