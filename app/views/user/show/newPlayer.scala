package views.html.user.show

import play.api.data.Form

import lidraughts.api.Context
import lidraughts.app.templating.Environment._
import lidraughts.app.ui.ScalatagsTemplate._
import lidraughts.user.User

import controllers.routes

object newPlayer {

  def apply(u: User)(implicit ctx: Context) =
    div(cls := "new_player")(
      h2("Welcome to lidraughts.org!"),
      p(
        "This is your profile page.",
        u.profile.isEmpty option frag(
          br,
          "Would you like to ",
          a(href := routes.Account.profile)("improve it"), "?"
        )
      ),
      p(
        if (u.kid) "Kid mode is enabled."
        else frag(
          "Will a child use this account? You might want to enable ", a(href := routes.Account.kid)("Kid mode"), "."
        )
      ),
      p(
        "What now? Here are a few suggestions:"
      ),
      ul(
        // li(a(href := routes.Learn.index)("Learn draughts rules")),
        li(a(href := routes.Puzzle.home)("Improve with draughts tactics puzzles")),
        li(a(href := s"${routes.Lobby.home}#ai")("Play the artificial intelligence")),
        li(a(href := s"${routes.Lobby.home}#hook")("Play opponents from around the world")),
        li(a(href := routes.User.list)("Follow your friends on lidraughts")),
        li(a(href := routes.Tournament.home(1))("Play in tournaments")),
        li("Learn from ", a(href := routes.Study.allDefault(1))("studies")),
        // " and ", a(href := routes.Video.index)("videos")),
        li(a(href := routes.Pref.form("game-display"))("Configure lidraughts to your liking")),
        li("Explore the site and have fun :)")
      )
    )
}
