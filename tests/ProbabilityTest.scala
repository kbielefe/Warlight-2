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

  it should "be 1 for n 100, k 100" in {
    Probability.binomial(100, 100) shouldBe 1
  }

  it should "be 1 for n 100, k 0" in {
    Probability.binomial(100, 0) shouldBe 1
  }

  "Attack Success" should "have probability zero for 1 attacker and 1 defender" in {
    Probability.attackSucceeds(1, 1) shouldBe 0.0
  }

  it should "have probability 1.0 for 2 attackers and 1 defender" in {
    Probability.attackSucceeds(2, 1) shouldBe 1.0
  }

  it should "be close to simulation for 10 attackers and 6 defenders" in {
    val simulation = ((1 to 1000) map {x => Probability.simulate(10, 6)}).sum / 1000
    math.abs(Probability.attackSucceeds(10, 6) - simulation) should be < 0.015
  }
}
