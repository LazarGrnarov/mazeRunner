import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MazeSpec extends AnyFlatSpec with Matchers {
  val point = MazeUtils.Point.fromTuple((0, 0))
  val point2 = MazeUtils.Point.fromTuple((1, 0))

  val mazeStr =
    """
@---A---+
        |
x-B-+   C
    |   |
    +---+
       """

  val missingStartStr =
    """
   -A---+
        |
x-B-+   C
    |   |
    +---+
"""

  val illegalCharStr =
    """///
   -A---+
        |
x-B-+   C
    |   |
    +---+
"""

  val forkStr =
  """
      x-B
        |
 @--A---+
        |
   x+   C
    |   |
    +---+
"""

  val missingEndStr =
"""
   @--A---+
          |
    B-+   C
      |   |
      +---+
  """

  val multiStartStr =
"""
 @--A-@-+
        |
x-B-+   C
    |   |
    +---+
"""

  val fakeTurnStr =
"""
@-A-+-B-x
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
  val missingStartMaze = MazeUtils.stringToMaze(missingStartStr)

  "MazeUtils" should "parse string as maze" in {
    parsedMaze.path shouldBe a[Map[MazeUtils.Point, MazeUtils.Symbol]]
  }

  it should "fail with Illegal character exception" in {
    assertThrows[Exceptions.IllegalCharException] {
      MazeUtils.stringToMaze(illegalCharStr)
    }
  }

  val mazeStrGoonies =
    """
    +-O-N-+
    |     |
    |   +-I-+
@-G-O-+ | | |
    | | +-+ E
    +-+     S
            |
            x
    """

  "MazeRunner" should "collect letters and path" in {
    val maze = MazeUtils.stringToMaze(mazeStrGoonies)
    val (letters, path) = MazeRunner.collectSymbols(maze)
    letters shouldBe "GOONIES"
    path shouldBe "@-G-O-+|+-+|O||+-O-N-+|I|+-+|+-I-+|ES|x"
  }

  it should "fail with no start found error" in {
    assertThrows[Exceptions.StartNotFoundException.type] {
      MazeRunner.collectSymbols(missingStartMaze)
    }
  }

  val forkMaze = MazeUtils.stringToMaze(forkStr)
  it should "fail with fork found" in {
    assertThrows[Exceptions.ForkFoundException.type] {
      MazeRunner.collectSymbols(forkMaze)
    }
  }

  val endMaze = MazeUtils.stringToMaze(missingEndStr)
  it should "fail with end not found" in {
    assertThrows[Exceptions.NoExitFoundException.type] {
      MazeRunner.collectSymbols(endMaze)
    }
  }

  val multiStartMaze = MazeUtils.stringToMaze(multiStartStr)
  it should "fail with too many starts" in {
    assertThrows[Exceptions.TooManyStartsException.type] {
      MazeRunner.collectSymbols(multiStartMaze)
    }
  }
  val fakeTurnMaze = MazeUtils.stringToMaze(fakeTurnStr)
  it should "fail with illegal path change" in {
    assertThrows[Exceptions.IllegalPathChangeException.type] {
      MazeRunner.collectSymbols(fakeTurnMaze)
    }
  }
}
