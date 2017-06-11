import streams._

object BlocksTest {


  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
    ls.foldLeft(startBlock) { case (block, move) => move match {
      case Left => block.left
      case Right => block.right
      case Up => block.up
      case Down => block.down
    }
    }
  }

  abstract class Level extends Solver with StringParserTerrain

  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  //lazy val p = Level0.pathsToGoal


  Level0.startPos





}