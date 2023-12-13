import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MazeSpec extends AnyFlatSpec with Matchers {
  val point = MazeUtils.Point.fromTuple((0,0))
  val point2 = MazeUtils.Point.fromTuple((1, 0))

  val mazeStr =
    """
@---A---+
        |
x-B-+   C
    |   |
    +---+
       """



  "Point" should "have row=0 col=0" in {
    assert(point.row == 0 && point.col == 0)
  }

  it should "inc col by 1 when moving right" in {
    val pointToRight = point + MazeUtils.Direction.Right
    assert(pointToRight.row == point.row && pointToRight.col == point.col + 1)
  }

  it should "inc row by 1 when moving Down" in {
    val pointToRight = point + MazeUtils.Direction.Down
    assert(pointToRight.row == point.row + 1 && pointToRight.col == point.col)
  }

  it should "stay the same when moving in opposite direction" in {
    val samePoint = (point + MazeUtils.Direction.Right) + MazeUtils.Direction.Left
    val samePoint2 = (point + MazeUtils.Direction.Up) + MazeUtils.Direction.Down
    assert(point == samePoint && point == samePoint2)
  }

  "Direction" should "be Down" in {
    val dir = point - point2
    assert(dir == MazeUtils.Direction.Down)
  }

  it should "be Up" in {
    val dir = point2 - point
    assert(dir == MazeUtils.Direction.Up)
  }
  val parsedMaze = MazeUtils.stringToMaze(mazeStr)

  "MazeUtils" should "parse string as maze" in {
    parsedMaze.path shouldBe a[Map[MazeUtils.Point, MazeUtils.Symbol]]
  }
}
