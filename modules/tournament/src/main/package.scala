package lila

package object tournament extends PackageObject {

  private[tournament] type Players = List[tournament.Player]

  private[tournament] type RankedPlayers = List[RankedPlayer]

  private[tournament] type Pairings = List[tournament.Pairing]

  private[tournament] type Waiting = Map[lila.user.User.ID, Int]

  private[tournament] val logger = lila.log("tournament")

  private[tournament] val pairingLogger = logger branch "pairing"
}
