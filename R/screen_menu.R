menu_screen_ui <- function() {
  # rad explorer menu
  bslib::page_fillable(
    title = "RADexplorer",
    shinyjs::useShinyjs(),
    fillable = FALSE,

    # app styles
    menu_styles(),

    # main card
    shiny::div(
      class = "menu-wrap",
      bslib::card(
        id = "taxaCard",
        class = "menu-card",
        bslib::card_body(
          class = "menu-card-body",

          # header
          shiny::h4("RADexplorer", style = "margin:0;"),

          # reference library
          bslib::card(
            class = "menu-section-card",
            shiny::div(
              class = "menu-row",
              shiny::tags$label(
                `for` = "ref_lib",
                "Reference library:",
                style = "margin:0; white-space:nowrap;"
              ),
              shiny::tags$select(
                id = "ref_lib",
                class = "form-control",
                disabled = "disabled",
                style = "width:160px;",
                shiny::tags$option(
                  value = "RADlib",
                  "RADlib v1.0",
                  selected = "selected"
                )
              )
            )
          ),

          # taxa select
          bslib::card(
            class = "menu-section-card",
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
                size = 8,
                `dropup-auto` = FALSE,
                container = "body"
              )
            )
          ),

          # selection note
          shiny::div(
            class = "menu-note",
            shiny::uiOutput("speciesNote")
          ),

          # action buttons
          shiny::div(
            class = "menu-button-row",
            shiny::actionButton("download", "Export RAD Databases", style = "flex:1;"),
            shiny::actionButton("continueWithTaxa", "Explore", style = "flex:1;")
          )
        )
      )
    )
  )
}

menu_styles <- function() {
  # shared styles
  shiny::tags$style(shiny::HTML("
    .menu-wrap{
      display:flex;
      align-items:flex-start;
      justify-content:center;
      padding-top:100px;
      padding-bottom:100px;
      overflow:visible
    }
    .menu-card{
      width:min(1100px,80vw);
      overflow:visible
    }
    .menu-card-body{
      display:flex;
      flex-direction:column;
      gap:16px;
      overflow:visible
    }
    .menu-section-card{
      overflow:visible
    }
    .menu-row{
      display:flex;
      align-items:center;
      gap:10px;
      overflow:visible
    }
    .menu-note{
      font-size:13px;
      margin-top:-12px
    }
    .menu-button-row{
      display:flex;
      gap:12px;
      width:100%
    }
  "))
}
