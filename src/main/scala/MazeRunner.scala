import Exceptions._
import MazeUtils.{Direction, Maze, Point, Position, Symbol, Tile}

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

  private def changeDirection(maze: Maze, position: Position): Position = {
    Direction.values.toList
      .filterNot(_ == position.direction)
      .filterNot(_ == Direction.Start)
      .filterNot(_ == Direction.inverse(position.direction))
      .flatMap(d => validSymbolDirection(maze, position.point, d).map(sym => (position.point + d, sym))) match {
      case ::((newPoint, _), next) =>
        if (next.nonEmpty) throw ForkFoundException
        Position(newPoint, position.point - newPoint)
      case Nil => throw NoExitFoundException
    }
  }

  private def next(maze: Maze, position: Position): Tile = {
    position.direction match {
      case Direction.Start =>
        start(maze)
      case _ =>
        validSymbolDirection(maze, position.point, position.direction) match {
          case Some(symbol) =>
            Tile(position.move, symbol)
          case None =>
            // need to change direction
            val currentSym = maze.path(position.point).value
            if (currentSym != '+' && currentSym != '@' && !('A' to 'Z').contains(currentSym)) {
              throw IllegalPathChangeException
            }
            val nextPosition = changeDirection(maze, position)
            Tile(nextPosition, maze.path.getOrElse(nextPosition.point, throw new Exception("Failed to change direction")))
        }
    }
  }

  private def start(maze: Maze): Tile = {
    maze.path.toList.filter(x => x._2.value == '@') match {
      case ::(head, next) =>
        if (next.nonEmpty) throw TooManyStartsException
        val startPos = Position(head._1, Direction.Start)
        val position = changeDirection(maze, startPos)
        Tile(position, maze.path(position.point))
      case Nil => throw StartNotFoundException
    }
  }

  def collectSymbols(maze: Maze) = {
    var strPath = "@"
    var letters = ""
    var letterSet = Set.empty[Point]
    var currentSym = Symbol.fromChar('@')
    var currentDir = Direction.Start
    var currentPoint = Point.fromTuple((0, 0))
    var currentPos = Position(currentPoint, currentDir)
    do {
      val nextTile = next(maze, currentPos)
      val sym = nextTile.symbol
      maze.path.get(currentPoint) match {
        case Some(testPoint) =>
          if (testPoint.value == '+' && nextTile.position.direction == currentDir) {
            // fake turn, error out
            throw IllegalPathChangeException
          }
        case None =>
      }
      strPath += sym.value
      if (('A' to 'Z').toList.contains(sym.value) && !letterSet.contains(nextTile.position.point)) {
        letterSet = letterSet ++ Set(nextTile.position.point)
        letters += sym.value
      }
      currentSym = sym
      currentDir = nextTile.position.direction
      currentPoint = nextTile.position.point
      currentPos = Position(currentPoint, currentDir)
    }
    while (currentSym.value != 'x')
    (letters, strPath)
  }
}
