import MazeRunner.collectSymbols
import MazeUtils.{Maze, stringToMaze}


object App extends App {

  val mazeStr =
    """
@---A---+
        |
x-B-+   C
    |   |
    +---+
       """

  val mazeStr2 =
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

  val mazeStr3 =
    """
 +-L-+
 |  +A-+
@B+ ++ H
 ++    x
    """

  val mazeStr4 =
    """
 @-A--+
      |
      +-B--x-C--D
    """


  val mazes = List(mazeStr, mazeStr2, mazeStr3, mazeStr4)
  mazes.foreach { mazeStr =>
    val maze = stringToMaze(mazeStr)
    Maze.printMaze(maze)
    val (letters, strPath) = collectSymbols(maze)
    println(s"PATH: $strPath")
    println(s"LETTERS: $letters")
  }

}
