case class GameState()

object Warlight {
  import scala.annotation.tailrec

  def processLine(state: GameState, line: String): GameState = {
    val fields = line split ' '

    fields(0) match {
      case _          => state
    }
  }

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val initialGameState = GameState()
    val state = lines.foldLeft(initialGameState)(processLine)
  }
}
