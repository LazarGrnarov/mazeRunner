import MazeUtils.{Direction, Maze, Point, Symbol}

import scala.util.{Failure, Success, Try}

object MazeRunner {
  private def validSymbolDirection(maze: Maze, point: Point, direction: Direction.Value): Option[Symbol] = {
    maze.path.get(point + direction) match {
      case Some(symbol) =>
        direction match {
          case Direction.Left | Direction.Right if symbol.value != '|' =>
            Some(symbol)
          case Direction.Up | Direction.Down if symbol.value != '-' =>
            Some(symbol)
          case Direction.Start => maze.path.get(point)
          case _ => None
        }
      case None => None
    }
  }

  private def changeDirection(maze: Maze, point: Point, direction: Direction.Value): (Point, Direction.Value) = {
    Direction.values.toList
      .filterNot(_ == direction)
      .filterNot(_ == Direction.Start)
      .filterNot(_ == Direction.inverse(direction))
      .flatMap(d => validSymbolDirection(maze, point, d).map(sym => (point + d, sym))) match {
      case ::((newPoint, _), next) =>
        if (next.nonEmpty) throw new Exception("Fork found, must only have one available direction change")
        (newPoint, point - newPoint)
      case Nil => throw new Exception("No exit found")
    }
  }

  private def next(maze: Maze, point: Point, direction: Direction.Value): (Point, Direction.Value, Symbol) = {
    direction match {
      case Direction.Start =>
        start(maze)
      case _ =>
        validSymbolDirection(maze, point, direction) match {
          case Some(symbol) =>
            (point + direction, direction, symbol)
          case None =>
            // need to change direction
            val currentSym = maze.path(point).value
            if (currentSym != '+' && currentSym != '@' && !('A' to 'Z').contains(currentSym)) {
              throw new Exception("Illegal path or change of direction")
            }
            val (step, changedDirection) = changeDirection(maze, point, direction)
            (step, changedDirection, maze.path.getOrElse(step, throw new Exception("Failed to change direction")))
        }
    }
  }

  private def start(maze: Maze): (Point, Direction.Value, Symbol) = {
    maze.path.toList.filter(x => x._2.value == '@') match {
      case ::(head, next) =>
        if (next.nonEmpty) throw new Exception("Too many starts found, must only have one available start")
        val (point, direction) = changeDirection(maze, head._1, Direction.Start)
        (point, direction, maze.path(point))
      case Nil => throw new Exception("No start found")
    }
  }

  def collectSymbols(maze: Maze) = {
    var strPath = "@"
    var letters = ""
    var letterSet = Set.empty[Point]
    var currentSym = Symbol.fromChar('@')
    var currentDir = Direction.Start
    var currentPoint = Point.fromTuple((0, 0))
    Try {
      do {
        val (point, direction, sym) = next(maze, currentPoint, currentDir)
        maze.path.get(currentPoint) match {
          case Some(testPoint) =>
            if (testPoint.value == '+' && direction == currentDir) {
              // fake turn, error out
              throw new Exception(s"Fake turn detected $testPoint[$currentPoint] -> $currentDir => $sym[$point] -> $direction")
            }
          case None =>
        }
        strPath += sym.value
        if (('A' to 'Z').toList.contains(sym.value) && !letterSet.contains(point)) {
          letterSet = letterSet ++ Set(point)
          letters += sym.value
        }
        currentSym = sym
        currentDir = direction
        currentPoint = point
      }
      while (currentSym.value != 'x')
    } match {
      case Failure(exception) => println(exception.getMessage)
      case Success(_) =>
    }
    (letters, strPath)
  }
}
