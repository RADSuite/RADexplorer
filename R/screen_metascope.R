metascope_screen_ui <- function(genus, species) {
  # Metascope download menu
  page_fillable(
    title = "RADport to Metascope",
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
        card(
          div(
            style = "display:flex; flex-direction:column; gap:0px;",
            checkboxInput(
              "metascopeFilter",
              label = HTML("Create Metascope filter files"),
              value = FALSE
            ),
            # genus selection
            tags$div(
              tags$label(
                `for` = "selectGenus",
                style = "display:block; margin-bottom:0px;",
                tags$div("Select genus or genera to analyze:"),
                tags$div(
                  style = "font-size:13px; margin-top:0px;",
                  class = "checkbox",
                  tags$label(
                    checkboxInput(
                      "entireGenus",
                      label = HTML("<i>Analyze all members of the selected genus</i>"),
                      value = FALSE
                    )
                  )
                )
              ),
              selectizeInput(
                "selectGenusFilter", label = NULL,
                choices = genus,
                multiple = TRUE,
                options = list(placeholder = "Type to search", maxOptions = 10000, openOnFocus = FALSE),
                width = "100%"
              )
            ),
            # species select - only shows up after you select a genus or genera
            conditionalPanel(
              condition = "input.selectGenusFilter && input.selectGenusFilter.length > 0",
              selectizeInput(
                "selectTaxaFilter", "Select species to analyze:",
                choices = character(0),
                multiple = TRUE,
                options = list(
                  placeholder = "Type to search",
                  maxOptions = 10000,
                  closeAfterSelect = FALSE,
                  openOnFocus = FALSE
                ),
                width = "100%"
              )
            )
          )
        ),
        card(
          fluidPage(
            useShinyjs(),
            div(
              style = "display:flex; gap:12px; width:100%;",
              actionButton("port", "Download", style = "flex:1;")
            )
          )
        )
      )
    )
  )
}
