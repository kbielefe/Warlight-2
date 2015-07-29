import org.scalatest._

class ProbabilityTest extends FlatSpec with Matchers {
  "Probability" should "predict 1 attacker lost for 1 defender" in {
    val (min, mean, max) = Probability.attackersLost(1)
    min  shouldBe 1
    mean shouldBe 1
    max  shouldBe 1
  }

  it should "predict correct attackers lost for 10 defenders" in {
    val (min, mean, max) = Probability.attackersLost(10)
    min  shouldBe 6
    mean shouldBe 7
    max  shouldBe 7
  }

  it should "predict correct attackers lost for 100 defenders" in {
    val (min, mean, max) = Probability.attackersLost(100)
    min  shouldBe 59 
    mean shouldBe 70
    max  shouldBe 75
  }

  it should "predict 1 defender lost for 1 attacker" in {
    val (min, mean, max) = Probability.defendersLost(1)
    min  shouldBe 1
    mean shouldBe 1
    max  shouldBe 1
  }

  it should "predict correct defenders lost for 10 attackers" in {
    val (min, mean, max) = Probability.defendersLost(10)
    min  shouldBe 5
    mean shouldBe 6
    max  shouldBe 7
  }

  it should "predict correct defenders lost for 100 attackers" in {
    val (min, mean, max) = Probability.defendersLost(100)
    min  shouldBe 50
    mean shouldBe 60
    max  shouldBe 66
  }

  "Binomial" should "be 161700 for n 100, k 3" in {
    Probability.binomial(100, 3) shouldBe 161700
  }
}
