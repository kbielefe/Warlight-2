case class GameState(map: GameMap, settings: Settings)

object Warlight {
  import scala.annotation.tailrec

  def processLine(state: GameState, line: String): GameState = {
    val fields = line split ' '

    fields(0) match {
      case "setup_map"            => GameState(state.map.setup(fields), state.settings)
      case "settings"             => GameState(state.map, state.settings.setup(fields))
      case "update_map"           => GameState(state.map.update(fields), state.settings)
      case "opponent_moves"       => state
      case "pick_starting_region" => state
      case "go"                   => state
      case _                      => state
    }
  }

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val initialGameState = GameState(GameMap(), Settings())
    val state = lines.foldLeft(initialGameState)(processLine)
  }
}
