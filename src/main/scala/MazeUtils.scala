object MazeUtils {

  case class Point(row: Int, col: Int) {
    def +(direction: Direction.Value) = {
      direction match {
        case Direction.Left => Point(row, col - 1)
        case Direction.Right => Point(row, col + 1)
        case Direction.Up => Point(row - 1, col)
        case Direction.Down => Point(row + 1, col)
        case Direction.Start => throw new Exception("Cannot use Start direction to move")
      }
    }

    def -(p: Point): Direction.Value = {
      if (row > p.row && col == p.col) Direction.Up
      else if (row < p.row && col == p.col) Direction.Down
      else if (row == p.row && col > p.col) Direction.Left
      else if (row == p.row && col < p.col) Direction.Right
      else throw new Exception("Diagonal change not allowed")
    }
  }

  def stringToMaze(s: String) = {
    val path = s.split("\n").toList.zipWithIndex.flatMap { case (rowStr, rowIndex) =>
      println(s"row[$rowIndex] -> $rowStr")
      rowStr.zipWithIndex.map { case (c, colIndex) => Point.fromTuple((rowIndex, colIndex)) -> c }
        .filterNot { case (_, c) => c.isWhitespace }.map { case (point, c) => (point, Symbol.fromChar(c)) }
    }.toMap
    Maze(path = path)
  }

  case class Maze(path: Map[Point, Symbol])

  case class Symbol(value: Char) extends AnyVal

  object Direction extends Enumeration {
    val Left, Right, Up, Down, Start = Value

    def inverse(d: Value) = d match {
      case Left => Right
      case Right => Left
      case Up => Down
      case Down => Up
      case Start => Start
    }
  }

  object Maze {
    def sort(maze: Maze): List[List[(Int, Symbol)]] = {
      maze.path.toList.groupBy { case (point, _) => point.row }.toList
        .sortBy { case (row, _) => row }
        .map { case (_, row) => row.sortBy(r => r._1.col).map(x => x._1.col -> x._2) }
    }

    def printMaze(maze: Maze) = {
      sort(maze).foreach { row =>
        val rowMap: Map[Int, Char] = row.map(x => x._1 -> x._2.value).toMap
        val rowLength = row.map(x => x._1).max
        println((0 to rowLength).map(n => rowMap.getOrElse(n, ' ')).mkString)
      }
    }
  }

  object Symbol {
    def fromChar(c: Char) = {
      if ((('A' to 'Z').toList ++ List('+', '-', '|', '@', 'x')).contains(c)) Symbol(c)
      else throw new Exception("Illegal Char")
    }
  }

  object Point {
    def fromTuple(tup: (Int, Int)) = {
      if (tup._1 >= 0 && tup._2 >= 0) Point(tup._1, tup._2)
      else throw new Exception("Coordinates must be zero or positive integers")
    }
  }

  def parse(rawMap: Map[(Int, Int), Char]): Maze = {
    val path = rawMap.map { case (point, c) =>
      Point.fromTuple(point) -> Symbol.fromChar(c)
    }
    Maze(path)
  }

}
