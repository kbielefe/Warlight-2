object Probability {
  val luckFactor = 0.16
  val attackerLostProbability = 0.7
  val defenderLostProbability = 0.6

  private def commonLost(opponents: Int, lostProbability: Double): (Int, Int, Int) = {
    val mean = opponents.toDouble * lostProbability
    val min = mean * (1.0 - luckFactor)
    val max = min + opponents.toDouble * luckFactor
    (math.round(min).toInt, math.round(mean).toInt, math.round(max).toInt)
  }

  def attackersLost(defenders: Int): (Int, Int, Int) = {
    commonLost(defenders, attackerLostProbability)
  }

  def defendersLost(attackers: Int): (Int, Int, Int) = {
    commonLost(attackers, defenderLostProbability)
  }

  def binomial(n: Int, k: Int): BigDecimal = {
    val bigN = BigDecimal(n)
    ((1 to k) map {i => (bigN + 1 - i) / i}).product
  }

  def attackSucceeds(attackers: Int, defenders: Int): Double = {
    0.0
  }
}
