package lila.tournament

import lila.user.User
import ornicar.scalalib.Zero

private[tournament] trait Ranking {
  def get(userId: User.ID): Option[Int]
  def size: Int
}

private[tournament] object Ranking {
  final val empty = new Ranking {
    def get(userId: User.ID) = None
    def size                 = 0
  }
  object implicits {
    implicit final val rankingZero: Zero[Ranking] = Zero.instance(empty)
  }
}

private[tournament] class FinishedRanking(m: Map[User.ID, Int]) extends Ranking {
  def get(userId: User.ID) = m get userId
  def size                 = m.size
}

case class UserIdWithMagicScore(userId: User.ID, magicScore: Int) extends Ordered[UserIdWithMagicScore] {
  override def compare(that: UserIdWithMagicScore) =
    if (magicScore > that.magicScore) -1
    else if (magicScore < that.magicScore) 1
    else userId compare that.userId
}

private class UpdatableRanking(
    magicScores: scala.collection.immutable.Map[User.ID, Int],
    ranks: scala.collection.immutable.TreeSet[UserIdWithMagicScore]
) {
  private[this] def updatedRanks(userId: User.ID, oldScore: Int, newScore: Int) =
    ranks excl UserIdWithMagicScore(userId, oldScore) incl UserIdWithMagicScore(userId, newScore)

  def updated(userId: User.ID, magicScore: Int): UpdatableRanking =
    magicScores
      .get(userId)
      .fold(
        new UpdatableRanking(
          magicScores.updated(userId, magicScore),
          ranks incl UserIdWithMagicScore(userId, magicScore)
        )
      )(oldScore => {
        if (oldScore != magicScore)
          new UpdatableRanking(
            magicScores.updated(userId, magicScore),
            updatedRanks(userId, oldScore, magicScore)
          )
        else this
      })
  def -(userId: User.ID) =
    magicScores
      .get(userId)
      .fold(this)(score =>
        new UpdatableRanking(magicScores - userId, ranks excl UserIdWithMagicScore(userId, score))
      )

  def getRank(userId: User.ID): Option[Int] =
    magicScores.get(userId).map(m => ranks.rangeUntil(UserIdWithMagicScore("", m)).size)

  def size = magicScores.size
}

private object UpdatableRanking {
  val empty = new UpdatableRanking(
    scala.collection.immutable.Map.empty[User.ID, Int],
    scala.collection.immutable.TreeSet.empty[UserIdWithMagicScore]
  )
  def from(coll: IterableOnce[UserIdWithMagicScore]) =
    coll.iterator.foldLeft(empty)((acc, x) => acc.updated(x.userId, x.magicScore))
}

private[tournament] class OngoingRanking(private var ranking: UpdatableRanking) extends Ranking {
  def synchronizedUpdate(userId: User.ID, magicScore: Int) =
    synchronized {
      ranking = ranking.updated(userId, magicScore)
    }
  def synchronizedRemove(userId: User.ID) =
    synchronized {
      ranking -= userId
    }
  def get(userId: User.ID) = ranking getRank userId
  def size                 = ranking.size
}

private[tournament] object OngoingRanking {
  def from(coll: IterableOnce[UserIdWithMagicScore]): OngoingRanking =
    new OngoingRanking(UpdatableRanking.from(coll))
}
