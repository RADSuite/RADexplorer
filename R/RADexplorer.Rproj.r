metascope_screen_ui <- function(genus, species) {
  # Metascope download menu
  page_fillable(
    title = "RADport to MetaScope",
    fillable = TRUE,
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:50px; padding-bottom:50px; height:100vh; overflow:hidden;",
      div(
        style = "display:flex; gap:24px; width:min(900px, 95vw); height:calc(100vh - 150px); overflow:hidden; align-items:stretch;",
        card(
          style = "flex:1 1 0; height:100%; overflow:hidden;",
          card_body(
            style = "height:100%; overflow:hidden; min-height:0;",
            div(
              style = "display:flex; flex-direction:column; gap:16px; height:100%;",
              div(
                style = "display:flex; gap:12px; width:100%;",
                h4("RADport", style = "margin:0;"),
                actionButton("backToMenu", "Back", style = "margin-left:auto;")
              ),
              div(
                h4("To create MetaScope filters, select taxa below."),
                p("If no filters are needed, leave the selection blank."),
                # genus selection
                tags$div(
                  tags$label(
                    `for` = "selectGenus",
                    style = "display:block; margin-bottom:0px;",
                    tags$div("Select genus or genera to filter:"),
                    tags$div(
                      style = "font-size:13px; margin-top:0px;",
                      class = "checkbox",
                      tags$label(
                        checkboxInput(
                          "entireGenus",
                          label = HTML("<i>Filter all members of the selected genus</i>"),
                          value = FALSE
                        )
                      )
                    )
                  ),
                  selectizeInput(
                    "selectGenusFilter", label = NULL,
                    choices = genus,
                    multiple = TRUE,
                    options = list(
                      placeholder = "Type to search",
                      maxOptions = 10000,
                      openOnFocus = FALSE
                    ),
                    width = "100%"
                  )
                ),
                # species select - only shows up after you select a genus or genera
                conditionalPanel(
                  condition = "input.selectGenusFilter && input.selectGenusFilter.length > 0",
                  selectizeInput(
                    "selectTaxaFilter", "Select species to filter:",
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
              ),
              div(
                style = "margin-top:auto;",
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
      )
    )
  )
}