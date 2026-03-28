metascope_screen_ui <- function(genus, species) {
  # Metascope download menu
  bslib::page_fillable(
    title = "RADport to MetaScope",
    fillable = TRUE,
    shinyjs::useShinyjs(),
    htmltools::div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:50px; padding-bottom:50px; overflow:visible;",
      htmltools::div(
        style = "display:flex; gap:24px; width:min(900px, 95vw); overflow:visible; align-items:stretch;",
        bslib::card(
          style = "flex:1 1 0; overflow:visible;",
          bslib::card_body(
            style = "overflow:visible;",
            htmltools::div(
              style = "display:flex; flex-direction:column; gap:16px; overflow:visible;",
              htmltools::div(
                style = "display:flex; gap:12px; width:100%;",
                htmltools::h4("RADport", style = "margin:0;"),
                shiny::actionButton("backToMenu", "Back", style = "margin-left:auto;")
              ),
              htmltools::div(
                style = "overflow:visible;",
                shiny::checkboxInput(
                  "useMetascopeFilters",
                  "Use MetaScope filters",
                  value = FALSE
                ),
                bslib::card(
                  id = "metascopeFilterCard",
                  style = "opacity:0.5; background-color:#f8f9fa; overflow:visible;",
                  htmltools::p("To create MetaScope filters, select taxa below."),
                  shinyjs::disabled(
                    htmltools::tags$div(
                      id = "metascopeFilterControls",
                      style = "overflow:visible;",

                      htmltools::tags$div(
                        style = "overflow:visible;",
                        htmltools::tags$label(
                          `for` = "selectGenusFilter",
                          style = "display:block; margin-bottom:0px;",
                          htmltools::tags$div("Select genus or genera to filter:"),
                          htmltools::tags$div(
                            style = "font-size:13px; margin-top:0px;",
                            class = "checkbox",
                            htmltools::tags$label(
                              shiny::checkboxInput(
                                "entireGenusFilter",
                                label = shiny::HTML("<i>Filter all members of the selected genus</i>"),
                                value = FALSE
                              )
                            )
                          )
                        ),
                        shiny::selectizeInput(
                          "selectGenusFilter",
                          label = NULL,
                          choices = genus,
                          multiple = TRUE,
                          options = list(
                            placeholder = "Type to search",
                            maxOptions = 10000,
                            openOnFocus = FALSE,
                            dropdownParent = "body"
                          ),
                          width = "100%"
                        )
                      ),

                      shiny::conditionalPanel(
                        condition = "input.useMetascopeFilters && input.selectGenusFilter && input.selectGenusFilter.length > 0",
                        htmltools::div(
                          style = "overflow:visible;",
                          shiny::selectizeInput(
                            "selectTaxaFilter",
                            "Select species to filter:",
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
                          )
                        )
                      )
                    )
                  )
                )
              ),
              htmltools::div(
                style = "display:flex; flex-direction:column; gap:12px; width:100%;",
                shiny::actionButton("port", "Download RADport files", style = "width:100%;"),
                shiny::actionButton(
                  "metascopeInstructionsButton",
                  "MetaScope RADport Instructions",
                  style = "width:100%;"
                )
              )
            )
          )
        )
      )
    )
  )
}
