menu_screen_ui <- function() {
  bslib::page_fillable(
    title = "RADexplorer",
    shinyjs::useShinyjs(),
    fillable = FALSE,
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
            shinyWidgets::pickerInput(
              inputId = "selectTaxa",
              label = "Select species to analyze:",
              choices = NULL,
              multiple = TRUE,

              width = "100%",
              options = shinyWidgets::pickerOptions(
                `live-search` = TRUE,
                `live-search-style` = "contains",
                `actions-box` = TRUE,
                `selected-text-format` = "count > 8",
                `live-search-placeholder` = "Type to search",
                `none-selected-text` = "Type to search",
                size = 10,
                `dropup-auto` = FALSE,
                container = "body"
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
