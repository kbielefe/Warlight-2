case class GameMap(superRegions: Map[Int, Int] = Map.empty[Int, Int],
                   regions:      Map[Int, Int] = Map.empty[Int, Int],
                   neighbors:    Map[Int, Set[Int]] = Map.empty[Int, Set[Int]],
                   wastelands:       Array[Int] = Array.empty[Int],
                   opponentRegions:  Array[Int] = Array.empty[Int]) {

  def setup(fields: Array[String]): GameMap = {
    val args = fields drop 2
    fields(1) match {
      case "super_regions"             => addSuperRegions(args)
      case "regions"                   => addRegions(args)
      case "neighbors"                 => addNeighbors(args)
      case "wastelands"                => addWastelands(args)
      case "opponent_starting_regions" => addOpponentStartingRegions(args)
      case _                           => this
    }
  }

  private def addSuperRegions(fields: Array[String]): GameMap = {
    val ints = fields map {_.toInt}
    val newSuperRegions = ints grouped 2 map {case Array(id, reward) => (id, reward)}
    GameMap(newSuperRegions.toMap, regions, neighbors, wastelands, opponentRegions)
  }

  private def addRegions(fields: Array[String]): GameMap = {
    val ints = fields map {_.toInt}
    val newRegions = ints grouped 2 map {case Array(region, superRegion) => (region, superRegion)}
    GameMap(superRegions, newRegions.toMap, neighbors, wastelands, opponentRegions)
  }

  private def addNeighbors(fields: Array[String]): GameMap = {
    def addReverse(map: Map[Int, Set[Int]], reverse: (Int, Int)): Map[Int, Set[Int]] = {
      val (region, neighbor) = reverse
      val previous = map.getOrElse(region, Set.empty[Int])
      map + (region -> (previous + neighbor))
    }
    val oneWay = (fields grouped 2).toList map {case Array(region, neighbors) => (region.toInt, (neighbors split "," map {_.toInt}).toSet)}
    val reverses = oneWay flatMap {case (region, neighbors) => neighbors map {(_, region)}}
    val newNeighbors = reverses.foldLeft(oneWay.toMap)(addReverse)
    GameMap(superRegions, regions, newNeighbors, wastelands, opponentRegions)
  }

  private def addWastelands(fields: Array[String]): GameMap = {
    val newWastelands = fields map {_.toInt}
    GameMap(superRegions, regions, neighbors, newWastelands, opponentRegions)
  }

  private def addOpponentStartingRegions(fields: Array[String]): GameMap = {
    val newOpponentRegions = fields map {_.toInt}
    GameMap(superRegions, regions, neighbors, wastelands, newOpponentRegions)
  }
}
