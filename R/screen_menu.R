#' Main RADexplorer menu UI
#'
#' Creates the main menu page for selecting taxa and launching analysis.
#'
#' @return A Shiny UI object.
#' @export
menu_screen_ui <- function() {
  bslib::page_fillable(
    title = "RADexplorer",
    shinyjs::useShinyjs(),
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:100px; padding-bottom:100px; overflow:visible;",
      bslib::card(
        id = "taxaCard",
        style = "width:min(1100px, 80vw); overflow:visible;",
        bslib::card_body(
          style = "display:flex; flex-direction:column; gap:16px; overflow:visible;",

          h4("RADexplorer", style = "margin:0;"),

          bslib::card(
            style = "overflow:visible;",
            div(
              style = "display:flex; align-items:center; gap:10px; overflow:visible;",
              tags$label(`for` = "ref_lib", "Reference library:", style = "margin:0; white-space:nowrap;"),
              tags$select(
                id = "ref_lib",
                class = "form-control",
                disabled = "disabled",
                style = "width:160px;",
                tags$option(value = "RADlib", "RADlib v1.0", selected = "selected")
              )
            )
          ),

          bslib::card(
            style = "overflow:visible;",
            div(
              style = "overflow:visible;",
              shiny::selectizeInput(
                "selectTaxa",
                "Select species to analyze:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Type to search",
                  maxOptions = 1000,
                  closeAfterSelect = FALSE,
                  hideSelected = FALSE,
                  openOnFocus = TRUE,
                  dropdownParent = "body",
                  render = I("{
                    option: function(item, escape) {
                      var label = item.label || item.text || '';
                      if (label.match(/ - All Species$/)) {
                        return '<div style=\"padding:2px 12px;\"><strong>' + escape(label) + '</strong></div>';
                      }
                      return '<div style=\"padding:2px 12px;\">' + escape(label) + '</div>';
                    }
                  }")
                ),
                width = "100%"
              )
            )
          ),
          div(
            style = "font-size:13px; margin-top:-12px;",
            shiny::uiOutput("speciesNote")
          ),
          div(
            style = "display:flex; gap:12px; width:100%;",
            shiny::actionButton("download", "Export RAD Databases", style = "flex:1;"),
            shiny::actionButton("continueWithTaxa", "Explore", style = "flex:1;")
          )
        )
      )
    )
  )
}
