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
    if (n == k || k == 0)
      return 1

    val bigN = BigDecimal(n)
    ((1 to k) map {i => (bigN + 1 - i) / i}).product
  }

  private def allDestroyed(k: Int, destroyers: Int, lostProbability: Double): Double = {
    val (min, mean, max) = commonLost(destroyers, lostProbability)

    if (k <= min)
      return 1

    if (k > max)
      return 0

    val minDouble = destroyers.toDouble * lostProbability * (1.0 - luckFactor)
    val luckMin = math.ceil((k.toDouble - 0.5 - minDouble) / luckFactor).toInt
    val p = BigDecimal(lostProbability)
    ((luckMin to destroyers) map {k => (binomial(destroyers, k) * p.pow(k) * (1 - p).pow(destroyers - k)).toDouble}).sum
  }

  def attackSucceeds(attackers: Int, defenders: Int): Double = {
    (1.0 - allDestroyed(attackers, defenders, attackerLostProbability)) *
           allDestroyed(defenders, attackers, defenderLostProbability)
  }

  def attackersNeeded(defenders: Int, successProbability: Double): Int = {
    (Iterator.from(1) find {attackers => attackSucceeds(attackers, defenders) >= successProbability}).get
  }

  def simulate(attackers: Int, defenders: Int): Double = {
    val luckDefendersDestroyed = (1 to attackers) count {x => util.Random.nextDouble() < defenderLostProbability}
    val luckAttackersDestroyed = (1 to defenders) count {x => util.Random.nextDouble() < attackerLostProbability}

    val defendersDestroyed = math.round(attackers * defenderLostProbability * (1 - luckFactor) + luckDefendersDestroyed * luckFactor)
    val attackersDestroyed = math.round(defenders * attackerLostProbability * (1 - luckFactor) + luckAttackersDestroyed * luckFactor)

    if (defendersDestroyed >= defenders && attackersDestroyed < attackers)
      1.0
    else
      0.0
  }
}
