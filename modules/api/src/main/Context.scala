package lila.api

import play.api.i18n.Lang
import play.api.mvc.RequestHeader

import lila.common.{ HTTPRequest, Nonce }
import lila.pref.Pref
import lila.user.{ BodyUserContext, HeaderUserContext, UserContext }

case class PageData(
    teamNbRequests: Int,
    nbChallenges: Int,
    nbNotifications: Int,
    pref: Pref,
    blindMode: Boolean,
    hasFingerprint: Boolean,
    hasClas: Boolean,
    inquiry: Option[lila.mod.Inquiry],
    nonce: Option[Nonce],
    error: Boolean = false
)

object PageData {

  def anon(req: RequestHeader, nonce: Option[Nonce], blindMode: Boolean = false) =
    PageData(
      teamNbRequests = 0,
      nbChallenges = 0,
      nbNotifications = 0,
      lila.pref.RequestPref fromRequest req,
      blindMode = blindMode,
      hasFingerprint = false,
      hasClas = false,
      inquiry = none,
      nonce = nonce
    )

  def error(req: RequestHeader, nonce: Option[Nonce]) = anon(req, nonce).copy(error = true)
}

sealed trait Context extends lila.user.UserContextWrapper {

  val userContext: UserContext
  val pageData: PageData

  def lang = userContext.lang

  def teamNbRequests  = pageData.teamNbRequests
  def nbChallenges    = pageData.nbChallenges
  def nbNotifications = pageData.nbNotifications
  def pref            = pageData.pref
  def blind           = pageData.blindMode
  def noBlind         = !blind
  def nonce           = pageData.nonce
  def hasClas         = pageData.hasClas

  def currentTheme = lila.pref.Theme(pref.theme)

  def currentTheme3d = lila.pref.Theme3d(pref.theme3d)

  def currentPieceSet = lila.pref.PieceSet(pref.pieceSet)

  def currentPieceSet3d = lila.pref.PieceSet3d(pref.pieceSet3d)

  def currentSoundSet = lila.pref.SoundSet(pref.soundSet)

  lazy val currentBg =
    if (pref.bg == Pref.Bg.TRANSPARENT) "transp"
    else if (pref.bg == Pref.Bg.LIGHT) "light"
    else if (pref.bg == Pref.Bg.SYSTEM) "system"
    else "dark"

  lazy val mobileApiVersion = Mobile.Api requestVersion req

  def isMobileApi = mobileApiVersion.isDefined

  lazy val isMobileBrowser = HTTPRequest isMobile req

  def requiresFingerprint = isAuth && !pageData.hasFingerprint

  def zoom: Int = {
    req.session get "zoom2" flatMap (_.toIntOption) map (_ - 100) filter (0 <=) filter (100 >=)
  } | 85

  def flash(name: String): Option[String] = req.flash get name
}

sealed abstract class BaseContext(
    val userContext: lila.user.UserContext,
    val pageData: PageData
) extends Context

final class BodyContext[A](
    val bodyContext: BodyUserContext[A],
    data: PageData
) extends BaseContext(bodyContext, data) {

  def body = bodyContext.body
}

final class HeaderContext(
    headerContext: HeaderUserContext,
    data: PageData
) extends BaseContext(headerContext, data)

object Context {

  def error(req: RequestHeader, lang: Lang, nonce: Option[Nonce]): HeaderContext =
    new HeaderContext(UserContext(req, none, none, lang), PageData.error(req, nonce))

  def apply(userContext: HeaderUserContext, pageData: PageData): HeaderContext =
    new HeaderContext(userContext, pageData)

  def apply[A](userContext: BodyUserContext[A], pageData: PageData): BodyContext[A] =
    new BodyContext(userContext, pageData)

  trait ToLang {
    implicit def ctxLang(implicit ctx: Context): Lang = ctx.lang
  }
}
