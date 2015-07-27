case class Settings(timebank: Int = 10000,
                    time_per_move: Int = 500,
                    max_rounds: Int = 0,
                    your_bot: String = "",
                    opponent_bot: String = "",
                    starting_armies: Int = 0,
                    starting_regions: Set[Int] = Set.empty[Int],
                    starting_pick_amount: Int = 0) {

  def setup(fields: Array[String]): Settings = {
    val args = fields drop 2
    fields(1) match {
      case "timebank"             => Settings(args(0).toInt, time_per_move, max_rounds,    your_bot, opponent_bot, starting_armies, starting_regions,           starting_pick_amount)
      case "time_per_move"        => Settings(timebank,      args(0).toInt, max_rounds,    your_bot, opponent_bot, starting_armies, starting_regions,           starting_pick_amount)
      case "max_rounds"           => Settings(timebank,      time_per_move, args(0).toInt, your_bot, opponent_bot, starting_armies, starting_regions,           starting_pick_amount)
      case "your_bot"             => Settings(timebank,      time_per_move, max_rounds,    args(0),  opponent_bot, starting_armies, starting_regions,           starting_pick_amount)
      case "opponent_bot"         => Settings(timebank,      time_per_move, max_rounds,    your_bot, args(0),      starting_armies, starting_regions,           starting_pick_amount)
      case "starting_armies"      => Settings(timebank,      time_per_move, max_rounds,    your_bot, opponent_bot, args(0).toInt,   starting_regions,           starting_pick_amount)
      case "starting_regions"     => Settings(timebank,      time_per_move, max_rounds,    your_bot, opponent_bot, starting_armies, (args map {_.toInt}).toSet, starting_pick_amount)
      case "starting_pick_amount" => Settings(timebank,      time_per_move, max_rounds,    your_bot, opponent_bot, starting_armies, starting_regions,           args(0).toInt)
      case _                      => this
    }
  }
}
