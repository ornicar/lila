package lila.tournament

import play.api.i18n.Lang
import scala.concurrent.duration._

import lila.hub.LightTeam.TeamID
import lila.memo._
import lila.memo.CacheApi._
import lila.user.User

final private[tournament] class Cached(
    playerRepo: PlayerRepo,
    pairingRepo: PairingRepo,
    tournamentRepo: TournamentRepo,
    cacheApi: CacheApi
)(implicit
    ec: scala.concurrent.ExecutionContext
) {

  val nameCache = cacheApi.sync[(Tournament.ID, Lang), Option[String]](
    name = "tournament.name",
    initialCapacity = 65536,
    compute = { case (id, lang) =>
      tournamentRepo byId id dmap2 { _.name()(lang) }
    },
    default = _ => none,
    strategy = Syncache.WaitAfterUptime(20 millis),
    expireAfter = Syncache.ExpireAfterAccess(20 minutes)
  )

  val onHomepage = cacheApi.unit[List[Tournament]] {
    _.refreshAfterWrite(2 seconds)
      .buildAsyncFuture(_ => tournamentRepo.onHomepage)
  }

  def ranking(tour: Tournament): Fu[Ranking] =
    if (tour.isFinished) finishedRanking get tour.id
    else ongoingRanking get tour.id

  private[tournament] val teamInfo =
    cacheApi[(Tournament.ID, TeamID), Option[TeamBattle.TeamInfo]](16, "tournament.teamInfo") {
      _.expireAfterWrite(5 seconds)
        .maximumSize(64)
        .buildAsyncFuture { case (tourId, teamId) =>
          playerRepo.teamInfo(tourId, teamId) dmap some
        }
    }

  // only applies to ongoing tournaments
  private[tournament] val ongoingRanking =
    cacheApi[Tournament.ID, OngoingRanking](64, "tournament.ongoingRanking") {
      _.expireAfterAccess(10 minutes)
        .buildAsyncFuture(playerRepo.computeOngoingRanking)
    }

  // only applies to finished tournaments
  private val finishedRanking = cacheApi[Tournament.ID, Ranking](1024, "tournament.finishedRanking") {
    _.expireAfterAccess(1 hour)
      .maximumSize(2048)
      .buildAsyncFuture(playerRepo.computeFinishedRanking)
  }

  object battle {

    val teamStanding =
      cacheApi[Tournament.ID, List[TeamBattle.RankedTeam]](8, "tournament.teamStanding") {
        _.expireAfterWrite(1 second)
          .buildAsyncFuture { id =>
            tournamentRepo teamBattleOf id flatMap {
              _ ?? { playerRepo.bestTeamIdsByTour(id, _) }
            }
          }
      }
  }

  private[tournament] object sheet {

    import arena.Sheet

    private case class SheetKey(
        tourId: Tournament.ID,
        userId: User.ID,
        version: Sheet.Version,
        streakable: Sheet.Streakable
    )

    def apply(tour: Tournament, userId: User.ID): Fu[Sheet] =
      cache.get(keyOf(tour, userId))

    def update(tour: Tournament, userId: User.ID): Fu[Sheet] = {
      val key = keyOf(tour, userId)
      cache.invalidate(key)
      cache.get(key)
    }

    private def keyOf(tour: Tournament, userId: User.ID) =
      SheetKey(
        tour.id,
        userId,
        Sheet versionOf tour.startsAt,
        if (tour.streakable) Sheet.Streaks else Sheet.NoStreaks
      )

    private def compute(key: SheetKey): Fu[Sheet] =
      pairingRepo.finishedByPlayerChronological(key.tourId, key.userId) map {
        arena.Sheet(key.userId, _, key.version, key.streakable)
      }

    private val cache = cacheApi[SheetKey, Sheet](8192, "tournament.sheet") {
      _.expireAfterAccess(3 minutes)
        .maximumSize(32768)
        .buildAsyncFuture(compute)
    }
  }

  private[tournament] val notableFinishedCache = cacheApi.unit[List[Tournament]] {
    _.refreshAfterWrite(15 seconds)
      .buildAsyncFuture(_ => tournamentRepo.notableFinished(20))
  }
}
