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
      case "pick_starting_region" => pickStartingRegion(state, fields(1).toLong, fields drop 2 map {_.toInt}); state
      case "go"                   => if (fields(1) == "place_armies") placeArmies(state, fields(2).toLong) else attack(state, fields(2).toLong); state
      case _                      => state
    }
  }

  def pickStartingRegion(state: GameState, time: Long, choices: Array[Int]): Unit = {
    println(choices.head)
  }

  def placeArmies(state: GameState, time: Long): Unit = {
    println("No moves")
  }

  def attack(state: GameState, time: Long): Unit = {
    println("No moves")
  }

  def main(args: Array[String]) {
    val lines = io.Source.stdin.getLines
    val initialGameState = GameState(GameMap(), Settings())
    val state = lines.foldLeft(initialGameState)(processLine)
  }
}
