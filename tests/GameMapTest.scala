import org.scalatest._

class GameMapTest extends FlatSpec with Matchers {
  "Game Map" should "parse sample neighbors correctly" in {
    val line = "setup_map neighbors 1 2,3,4 2 3 4 5"
    val fields = line split ' '
    val initialMap = GameMap()
    val resultMap = initialMap.setup(fields)
    val expectedNeighbors = Map(1 -> Set(2, 3, 4), 2 -> Set(1, 3), 3 -> Set(1, 2), 4 -> Set(1, 5), 5 -> Set(4))
    resultMap.neighbors shouldBe expectedNeighbors
  }
}
