radport_screen_ui <- function() {
  # RADport download menu
  page_fillable(
    title = "RADport",
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:100px; padding-bottom:100px;",
      card(
        id = "taxaCard",
        style = "width: min(1100px, 80vw); max-height: 250vh; overflow: visible;",
        card_body(
          style = "display:flex; flex-direction:column; gap:16px; overflow: visible;",
          div(
            style = "display:flex; gap:12px; width:100%;",
            h4("RADport", style = "margin:0;"),
            actionButton("backToMenu", "Back", style = "margin-left:auto;")
          )
        ),
        # this is the card that provides the download options
        card(
          fluidPage(
            useShinyjs(),
            p("Select pipeline:"),
            div(
              style = "display:flex; gap:12px; width:100%;",
              actionButton("continueKraken", "Kraken", style = "flex:1;"),
              actionButton("continueMetascope", "Metascope", style = "flex:1;"),
              actionButton("continueQIIME", "QIIME2", style = "flex:1;")
            )
          )
        )
      )
    )
  )
}
