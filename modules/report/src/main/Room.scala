package lila.report

import lila.user.Holder

sealed trait Room {

  def key = toString.toLowerCase

  def name = toString
}

object Room {

  case object Cheat   extends Room
  case object Boost   extends Room
  case object Print   extends Room
  case object Playban extends Room
  case object Comm    extends Room
  case object Other   extends Room
  case object Xfiles  extends Room {
    override def name = "X-Files"
  }

  val all: List[Room] = List(Cheat, Boost, Print, Playban, Comm, Other, Xfiles)
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val allButXfiles: List[Room] = all.filter(Xfiles !=)

  implicit val roomIso = lila.common.Iso[String, Room](k => byKey.getOrElse(k, Other), _.key)

  def apply(key: String): Option[Room] = byKey get key

  def apply(reason: Reason): Room =
    reason match {
      case Reason.Cheat                        => Cheat
      case Reason.Boost                        => Boost
      case Reason.AltPrint | Reason.CheatPrint => Print
      case Reason.Playbans                     => Playban
      case Reason.Comm                         => Comm
      case Reason.Other                        => Other
    }

  def toReasons(room: Room): Set[Reason] =
    room match {
      case Cheat   => Set(Reason.Cheat)
      case Boost   => Set(Reason.Boost)
      case Print   => Set(Reason.AltPrint)
      case Playban => Set(Reason.Playbans)
      case Comm    => Set(Reason.Comm)
      case Other   => Set(Reason.Other)
      case Xfiles  => Set.empty
    }

  case class Scores(value: Map[Room, Int]) {
    def get     = value.get _
    def highest = ~value.values.maxOption
  }

  def isGrantedFor(mod: Holder)(room: Room) = {
    import lila.security.Granter
    room match {
      case Cheat   => Granter.is(_.MarkEngine)(mod)
      case Boost   => Granter.is(_.MarkBooster)(mod)
      case Print   => Granter.is(_.Admin)(mod)
      case Playban => Granter.is(_.Admin)(mod)
      case Comm    => Granter.is(_.Shadowban)(mod)
      case Other   => Granter.is(_.Admin)(mod)
      case Xfiles  => Granter.is(_.MarkEngine)(mod)
    }
  }
}
