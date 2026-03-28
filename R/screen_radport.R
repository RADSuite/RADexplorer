radport_screen_ui <- function() {
  # RADport download menu
  bslib::page_fillable(
    title = "RADport",
    htmltools::div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:100px; padding-bottom:100px;",
      bslib::card(
        id = "taxaCard",
        style = "width: min(1100px, 80vw); max-height: 250vh; overflow: visible;",
        bslib::card_body(
          style = "display:flex; flex-direction:column; gap:16px; overflow: visible;",
          htmltools::div(
            style = "display:flex; gap:12px; width:100%;",
            htmltools::h4("RADport", style = "margin:0;"),
            shiny::actionButton("backToMenu", "Back", style = "margin-left:auto;")
          )
        ),
        # this is the card that provides the download options
        bslib::card(
          shiny::fluidPage(
            shinyjs::useShinyjs(),
            htmltools::p("Select pipeline:"),
            htmltools::div(
              style = "display:flex; gap:12px; width:100%;",
              shiny::actionButton("continueKraken", "Kraken", style = "flex:1;"),
              shiny::actionButton("continueMetascope", "Metascope", style = "flex:1;"),
              shiny::actionButton("continueQIIME", "QIIME2", style = "flex:1;")
            )
          )
        )
      )
    )
  )
}
