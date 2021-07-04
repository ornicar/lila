package lila.tournament
import org.specs2.mutable.Specification

class RankingTest extends Specification {
  "ranking" should {
    "hand tests" in {
      val r = OngoingRanking.from(
        List(UserIdWithMagicScore("a", 5), UserIdWithMagicScore("b", 3), UserIdWithMagicScore("c", 7))
      )
      r.get("a") must_=== 1.some
      r.get("b") must_=== 2.some
      r.get("c") must_=== 0.some
      r.get("d") must_=== None
      r.synchronizedUpdate("a", 10)
      r.get("a") must_=== 0.some
      r.synchronizedRemove("a")
      r.get("a") must_=== None
      r.get("b") must_=== 1.some
    }
  }
}
