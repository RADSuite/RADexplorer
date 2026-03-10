menu_screen_ui <- function(genus) {
  # taxa select menu
  page_fillable(
    title = "RADexplorer",
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:100px; padding-bottom:100px;",
      card(
        id = "taxaCard",
        style = "width: min(1100px, 80vw); max-height: 250vh; overflow: visible;",
        card_body(
          style = "display:flex; flex-direction:column; gap:16px;",
          div(
            style = "display:flex; gap:12px; width:100%;",
            h4("RADexplorer", style = "margin:0;")
          ),
          # this is the Reference Library card
          # it currently grays out the option to select the library and forces the user to select RADlib
          card(
            div(
              style = "display:flex; align-items:center; gap:10px;",
              tags$label(`for` = "ref_lib", "Reference library:", style = "margin:0; white-space:nowrap;"),
              tags$select(
                id = "ref_lib",
                class = "form-control",
                disabled = "disabled",
                # sets the width of the drop down button - a little neurotic, can be removed lol
                style = "width: 160px; flex: 0 0 100px;",
                tags$option(value = "RADlib", "RADlib v1.0", selected = "selected")
              )
            )
          ),
          card(
            div(
              style = "display:flex; flex-direction:column; gap:0px;",
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
                  "selectGenus", label = NULL,
                  choices = genus,
                  multiple = TRUE,
                  options = list(placeholder = "Type to search", maxOptions = 10000, openOnFocus = FALSE),
                  width = "100%"
                )
              ),
              # species select - only shows up after you select a genus or genera
              conditionalPanel(
                condition = "input.selectGenus && input.selectGenus.length > 0",
                selectizeInput(
                  "selectTaxa", "Select species to analyze:",
                  choices = character(0),
                  multiple = TRUE,
                  options = list(
                    placeholder = "Type to search",
                    maxOptions = 10000,
                    closeAfterSelect = FALSE,
                    openOnFocus = FALSE
                  ),
                  width = "100%"
                ),
                div(
                  style = "font-size: 13px; margin-top:-6px;",
                  uiOutput("speciesNote")
                )
              )
            )
          ),

          fluidPage(
            useShinyjs(),
            div(
              style = "display:flex; gap:12px; width:100%;",
              actionButton("download", "Download", style = "flex:1;"),
              actionButton("continueWithTaxa", "Explore", style = "flex:1;")
            )
          )
        )
      )
    )
  )
}
