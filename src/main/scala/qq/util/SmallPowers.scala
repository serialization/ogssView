package qq.util

import scala.language.postfixOps

object SmallPowers {
  implicit class SmallPowersInt(x: Int) {
    def `²`: Int = x * x
    def `³`: Int = x * x `²`
    def `⁴`: Int = (x `²`) `²`
    def `⁵`: Int = x * x `⁴`
  }
  implicit class SmallPowersFloat(x: Float) {
    def `²`: Float = x * x
    def `³`: Float = x * x `²`
    def `⁴`: Float = (x `²`) `²`
    def `⁵`: Float = x * x `⁴`
  }
}
