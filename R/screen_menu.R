menu_screen_ui <- function() {
  # taxa select menu
  page_fillable(
    title = "RADexplorer",
    fillable = TRUE,
    useShinyjs(),
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:100px; padding-bottom:100px; overflow:visible;",
      card(
        id = "taxaCard",
        style = "width:min(1100px, 80vw); max-height:250vh; overflow:visible;",
        card_body(
          style = "display:flex; flex-direction:column; gap:16px; overflow:visible;",
          div(
            style = "display:flex; gap:12px; width:100%;",
            h4("RADexplorer", style = "margin:0;")
          ),
          # this is the Reference Library card
          # it currently grays out the option to select the library and forces the user to select RADlib
          card(
            style = "overflow:visible;",
            div(
              style = "display:flex; align-items:center; gap:10px; overflow:visible;",
              tags$label(`for` = "ref_lib", "Reference library:", style = "margin:0; white-space:nowrap;"),
              tags$select(
                id = "ref_lib",
                class = "form-control",
                disabled = "disabled",
                # sets the width of the drop down button - a little neurotic, can be removed lol
                style = "width:160px; flex:0 0 100px;",
                tags$option(value = "RADlib", "RADlib v1.0", selected = "selected")
              )
            )
          ),
          card(
            style = "overflow:visible;",
            div(
              style = "display:flex; flex-direction:column; gap:0px; overflow:visible;",
              div(
                style = "overflow:visible;",
                selectizeInput(
                  "selectTaxa",
                  "Select species to analyze:",
                  choices = character(0),
                  multiple = TRUE,
                  options = list(
                    placeholder = "Type to search",
                    maxOptions = 10000,
                    closeAfterSelect = FALSE,
                    openOnFocus = FALSE,
                    dropdownParent = "body"
                  ),
                  width = "100%"
                ),
                div(
                  style = "font-size:13px; margin-top:-6px;",
                  uiOutput("speciesNote")
                )
              )
            )
          ),

          div(
            style = "display:flex; gap:12px; width:100%;",
            actionButton("download", "Export RAD Databases", style = "flex:1;"),
            actionButton("continueWithTaxa", "Explore", style = "flex:1;")
          )
        )
      )
    )
  )
}
