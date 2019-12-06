package views.html.site

import lila.api.Context
import lila.app.templating.Environment._
import lila.app.ui.ScalatagsTemplate._

import controllers.routes

object offline {

  def apply()(implicit ctx: Context) = views.html.base.layout(
    title = "Offline",
    moreJs = embedJsUnsafe("""$('#try-again').click(() => window.location.reload());""")
  ) {
      main(cls := "page-small box box-pad")(
        h1("You are offline"),
        p(
          a(id := "try-again")("Try again!")
        ),
        p(
          "You can still use the ",
          a(href := routes.Editor.index)("editor"), "."
        )
      )
    }
}
