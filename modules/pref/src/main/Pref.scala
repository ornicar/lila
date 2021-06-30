package lila.pref

case class Pref(
    _id: String, // user id
    bg: Int,
    bgImg: Option[String],
    is3d: Boolean,
    theme: String,
    pieceSet: String,
    theme3d: String,
    pieceSet3d: String,
    soundSet: String,
    blindfold: Int,
    autoQueen: Int,
    autoThreefold: Int,
    takeback: Int,
    moretime: Int,
    clockTenths: Int,
    clockBar: Boolean,
    clockSound: Boolean,
    premove: Boolean,
    animation: Int,
    captured: Boolean,
    follow: Boolean,
    highlight: Boolean,
    destination: Boolean,
    coords: Int,
    replay: Int,
    challenge: Int,
    message: Int,
    studyInvite: Int,
    coordColor: Int,
    submitMove: Int,
    confirmResign: Int,
    mention: Boolean,
    insightShare: Int,
    keyboardMove: Int,
    zen: Int,
    rookCastle: Int,
    moveEvent: Int,
    pieceNotation: Int,
    resizeHandle: Int,
    tags: Map[String, String] = Map.empty
) {

  import Pref._

  def id = _id

  def realTheme      = Theme(theme)
  def realPieceSet   = PieceSet(pieceSet)
  def realTheme3d    = Theme3d(theme3d)
  def realPieceSet3d = PieceSet3d(pieceSet3d)

  def themeColor = if (bg == Bg.LIGHT) "#dbd7d1" else "#2e2a24"

  def realSoundSet = SoundSet(soundSet)

  def coordColorName = Color.choices.toMap.get(coordColor).fold("random")(_.toLowerCase)
  def coordsClass    = Coords classOf coords

  def hasDgt = tags contains Tag.dgt

  def set(name: String, value: String): Option[Pref] =
    name match {
      case "bg"    => Pref.Bg.fromString.get(value).map { bg => copy(bg = bg) }
      case "bgImg" => copy(bgImg = value.some).some
      case "theme" =>
        Theme.allByName get value map { t =>
          copy(theme = t.name)
        }
      case "pieceSet" =>
        PieceSet.allByName get value map { p =>
          copy(pieceSet = p.name)
        }
      case "theme3d" =>
        Theme3d.allByName get value map { t =>
          copy(theme3d = t.name)
        }
      case "pieceSet3d" =>
        PieceSet3d.allByName get value map { p =>
          copy(pieceSet3d = p.name)
        }
      case "is3d" => copy(is3d = value == "true").some
      case "soundSet" =>
        SoundSet.allByKey get value map { s =>
          copy(soundSet = s.key)
        }
      case "zen" => copy(zen = if (value == "1") 1 else 0).some
      case _     => none
    }

  def animationMillis: Int =
    animation match {
      case Animation.NONE   => 0
      case Animation.FAST   => 120
      case Animation.NORMAL => 250
      case Animation.SLOW   => 500
      case _                => 250
    }

  def isBlindfold = blindfold == Pref.Blindfold.YES

  def bgImgOrDefault = bgImg | Pref.defaultBgImg

  def pieceNotationIsLetter = pieceNotation == PieceNotation.LETTER

  def isZen = zen == Zen.YES

  def is2d = !is3d

  // atob("aHR0cDovL2NoZXNzLWNoZWF0LmNvbS9ob3dfdG9fY2hlYXRfYXRfbGljaGVzcy5odG1s")
  def botCompatible =
    theme == "brown" &&
      pieceSet == "cburnett" &&
      is2d &&
      animation == Animation.NONE &&
      highlight &&
      coords == Coords.OUTSIDE
}

object Pref {

  val defaultBgImg = "//lichess1.org/assets/images/background/landscape.jpg"

  trait BooleanPref {
    val NO      = 0
    val YES     = 1
    val choices = Seq(NO -> "No", YES -> "Yes")
  }

  object BooleanPref {
    val verify = (v: Int) => v == 0 || v == 1
  }

  object Bg {
    val LIGHT       = 100
    val DARK        = 200
    val DARKBOARD   = 300
    val TRANSPARENT = 400
    val SYSTEM      = 500

    val choices = Seq(
      LIGHT       -> "Light",
      DARK        -> "Dark",
      DARKBOARD   -> "Dark Board",
      TRANSPARENT -> "Transparent",
      SYSTEM      -> "System Default"
    )

    val fromString = Map(
      "light"     -> LIGHT,
      "dark"      -> DARK,
      "darkBoard" -> DARKBOARD,
      "transp"    -> TRANSPARENT,
      "system"    -> SYSTEM
    )

    val asString = fromString.map(_.swap)
  }

  object Tag {
    val dgt = "dgt"
  }

  object Color {
    val WHITE  = 1
    val RANDOM = 2
    val BLACK  = 3

    val choices = Seq(
      WHITE  -> "White",
      RANDOM -> "Random",
      BLACK  -> "Black"
    )
  }

  object AutoQueen {
    val NEVER   = 1
    val PREMOVE = 2
    val ALWAYS  = 3

    val choices = Seq(
      NEVER   -> "Never",
      ALWAYS  -> "Always",
      PREMOVE -> "When premoving"
    )
  }

  object SubmitMove {
    val NEVER                    = 0
    val CORRESPONDENCE_ONLY      = 4
    val CORRESPONDENCE_UNLIMITED = 1
    val ALWAYS                   = 2

    val choices = Seq(
      NEVER                    -> "Never",
      CORRESPONDENCE_ONLY      -> "Correspondence games only",
      CORRESPONDENCE_UNLIMITED -> "Correspondence and unlimited",
      ALWAYS                   -> "Always"
    )
  }

  object ConfirmResign extends BooleanPref

  object InsightShare {
    val NOBODY    = 0
    val FRIENDS   = 1
    val EVERYBODY = 2

    val choices = Seq(
      NOBODY    -> "With nobody",
      FRIENDS   -> "With friends",
      EVERYBODY -> "With everybody"
    )
  }

  object Mention extends BooleanPref

  object KeyboardMove extends BooleanPref

  object RookCastle {
    val NO  = 0
    val YES = 1

    val choices = Seq(
      NO  -> "Castle by moving by two squares",
      YES -> "Castle by moving onto the rook"
    )
  }

  object MoveEvent {
    val CLICK = 0
    val DRAG  = 1
    val BOTH  = 2

    val choices = Seq(
      CLICK -> "Click two squares",
      DRAG  -> "Drag a piece",
      BOTH  -> "Both clicks and drag"
    )
  }

  object PieceNotation {
    val SYMBOL = 0
    val LETTER = 1

    val choices = Seq(
      SYMBOL -> "Chess piece symbol",
      LETTER -> "PGN letter (K, Q, R, B, N)"
    )
  }

  object Blindfold extends BooleanPref {
    override val choices = Seq(
      NO  -> "What? No!",
      YES -> "Yes, hide the pieces"
    )
  }

  object AutoThreefold {
    val NEVER  = 1
    val TIME   = 2
    val ALWAYS = 3

    val choices = Seq(
      NEVER  -> "Never",
      ALWAYS -> "Always",
      TIME   -> "When time remaining < 30 seconds"
    )
  }

  object Takeback {
    val NEVER  = 1
    val CASUAL = 2
    val ALWAYS = 3

    val choices = Seq(
      NEVER  -> "Never",
      ALWAYS -> "Always",
      CASUAL -> "In casual games only"
    )
  }

  object Moretime {
    val NEVER  = 1
    val CASUAL = 2
    val ALWAYS = 3

    val choices = Seq(
      NEVER  -> "Never",
      ALWAYS -> "Always",
      CASUAL -> "In casual games only"
    )
  }

  object Animation {
    val NONE   = 0
    val FAST   = 1
    val NORMAL = 2
    val SLOW   = 3

    val choices = Seq(
      NONE   -> "None",
      FAST   -> "Fast",
      NORMAL -> "Normal",
      SLOW   -> "Slow"
    )
  }

  object Coords {
    val NONE    = 0
    val INSIDE  = 1
    val OUTSIDE = 2

    val choices = Seq(
      NONE    -> "No",
      INSIDE  -> "Inside the board",
      OUTSIDE -> "Outside the board"
    )

    def classOf(v: Int) =
      v match {
        case INSIDE  => "in"
        case OUTSIDE => "out"
        case _       => "no"
      }
  }

  object Replay {
    val NEVER  = 0
    val SLOW   = 1
    val ALWAYS = 2

    val choices = Seq(
      NEVER  -> "Never",
      SLOW   -> "On slow games",
      ALWAYS -> "Always"
    )
  }

  object ClockTenths {
    val NEVER   = 0
    val LOWTIME = 1
    val ALWAYS  = 2

    val choices = Seq(
      NEVER   -> "Never",
      LOWTIME -> "When time remaining < 10 seconds",
      ALWAYS  -> "Always"
    )
  }

  object Challenge {
    val NEVER  = 1
    val RATING = 2
    val FRIEND = 3
    val ALWAYS = 4

    val ratingThreshold = 300

    val choices = Seq(
      NEVER  -> "Never",
      RATING -> s"If rating is ± $ratingThreshold",
      FRIEND -> "Only friends",
      ALWAYS -> "Always"
    )
  }

  object Message {
    val NEVER  = 1
    val FRIEND = 2
    val ALWAYS = 3

    val choices = Seq(
      NEVER  -> "Only existing conversations",
      FRIEND -> "Only friends",
      ALWAYS -> "Always"
    )
  }

  object StudyInvite {
    val NEVER  = 1
    val FRIEND = 2
    val ALWAYS = 3

    val choices = Seq(
      NEVER  -> "Never",
      FRIEND -> "Only friends",
      ALWAYS -> "Always"
    )
  }

  object ResizeHandle {
    val NEVER   = 0
    val INITIAL = 1
    val ALWAYS  = 2

    val choices = Seq(
      NEVER   -> "Never",
      INITIAL -> "On initial position",
      ALWAYS  -> "Always"
    )
  }

  object Zen extends BooleanPref {}

  def create(id: String) = default.copy(_id = id)

  lazy val default = Pref(
    _id = "",
    bg = Bg.SYSTEM,
    bgImg = none,
    is3d = false,
    theme = Theme.default.name,
    pieceSet = PieceSet.default.name,
    theme3d = Theme3d.default.name,
    pieceSet3d = PieceSet3d.default.name,
    soundSet = SoundSet.default.name,
    blindfold = Blindfold.NO,
    autoQueen = AutoQueen.PREMOVE,
    autoThreefold = AutoThreefold.TIME,
    takeback = Takeback.ALWAYS,
    moretime = Moretime.ALWAYS,
    clockBar = true,
    clockSound = true,
    premove = true,
    animation = Animation.NORMAL,
    captured = true,
    follow = true,
    highlight = true,
    destination = true,
    coords = Coords.INSIDE,
    replay = Replay.ALWAYS,
    clockTenths = ClockTenths.LOWTIME,
    challenge = Challenge.ALWAYS,
    message = Message.ALWAYS,
    studyInvite = StudyInvite.ALWAYS,
    coordColor = Color.RANDOM,
    submitMove = SubmitMove.CORRESPONDENCE_ONLY,
    confirmResign = ConfirmResign.YES,
    mention = true,
    insightShare = InsightShare.FRIENDS,
    keyboardMove = KeyboardMove.NO,
    zen = Zen.NO,
    rookCastle = RookCastle.YES,
    moveEvent = MoveEvent.BOTH,
    pieceNotation = PieceNotation.SYMBOL,
    resizeHandle = ResizeHandle.INITIAL,
    tags = Map.empty
  )

  import ornicar.scalalib.Zero
  implicit def PrefZero: Zero[Pref] = Zero.instance(default)
}
