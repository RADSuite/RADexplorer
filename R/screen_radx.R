radx_screen_ui <- function() {
  # rad explorer menu
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      # header
      shiny::div(
        style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
        shiny::h5("Options", style = "margin:0;"),
        shiny::actionButton("backToMenu", "Back")
      ),

      # variable region select
      shiny::div(
        class = "vr-grid",
        shiny::checkboxGroupInput(
          "varRegions", "Select v-regions:",
          choices = paste0("V", 1:9), selected = "V4"
        )
      ),
      shiny::div(
        style = "margin:-16px 0 8px;font-size:12px;",
        shiny::actionLink("deselectVarRegions", "Deselect all v-regions")
      ),

      # display options
      bslib::input_switch("detailedView", "Detailed View", TRUE),
      bslib::input_switch("vregionIDs", "V-Region Labels", FALSE),

      # taxa search
      shinyWidgets::pickerInput(
        "radxSearchTaxa", "Locate Taxa:",
        choices = NULL, multiple = TRUE, width = "100%",
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
      ),
      shiny::actionButton("searchTaxa", "Locate", style = "width:100%;margin-bottom:10px;")
    ),

    # app styles
    radx_styles(),

    # instructions
    radx_instructions(),

    # main plot
    radx_loading_plot("visual")

  )
}

radx_loading_plot <- function(id, height = "570px") {
  # plot output with loading overlay
  shiny::div(
    class = "radx-wrap",
    shiny::div(class = "radx-out", plotly::plotlyOutput(id, height = height)),
    shiny::div(
      class = "radx-load",
      shiny::div(class = "radx-spin"),
      shiny::div(class = "radx-text", "Loading visualization...")
    )
  )
}

radx_instructions <- function() {
  # radx instructions accordion
  bslib::accordion(
    id = "radx_instructions",
    open = TRUE,
    class = "radx-accordion",
    bslib::accordion_panel(
      "RADx Instructions",
      shiny::p(
        class = "radx-instructions-text",
        "Select the 16S rRNA gene variable regions you wish to study in the left sidebar.",
        shiny::tags$br(),
        "  • A green checkmark (", shiny::span("✓", style = "color:green;font-weight:bold;"),
        ") indicates that a taxon can be identified using the selected v-region(s).",
        shiny::tags$br(),
        "  • A red bracket (", shiny::span("[", style = "color:red;font-weight:bold;"),
        ") groups taxa that cannot be distinguished from one another with the selected v-region(s).",
        shiny::tags$br(),
        shiny::tags$br(),
        "Use Locate to show the chosen taxa and any taxa they are grouped with.",
        shiny::tags$br(),
        "  • Located taxa will be marked with a blue arrow (",
        shiny::span("➤", style = "color:#00dfeb;font-size:14pt;font-weight:bold;"),
        ").",
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::tags$small(
          style = "display:block;font-style:italic;margin-top:6px;",
          "Note: Colors designate identical sequences within a v-region. Colors should not be compared across columns."
        )
      )
    )
  )
}

radx_styles <- function() {
  # shared styles
  shiny::tags$style(shiny::HTML("
    .vr-grid .shiny-options-group{display:grid;grid-template-columns:repeat(3,1fr);gap:6px 12px}
    .vr-grid .checkbox{margin:0}
    .radx-wrap{position:relative;min-height:570px}
    .radx-out{transition:opacity .15s ease}
    .radx-load{
      display:none;position:absolute;inset:0;z-index:10;
      background:rgba(255,255,255,.9);
      align-items:center;justify-content:center;flex-direction:column;
      gap:16px;text-align:center
    }
    .radx-wrap:has(.radx-out .recalculating) .radx-load{display:flex}
    .radx-wrap:has(.radx-out .recalculating) .radx-out{opacity:0}
    .radx-spin{
      width:48px;height:48px;border-radius:50%;
      border:5px solid #d9d9d9;border-top-color:#2c7c31;
      animation:radx-spin .8s linear infinite
    }
    .radx-text{font-size:15px;color:#444}
    @keyframes radx-spin{to{transform:rotate(360deg)}}
    .radx-accordion .accordion-button{
      padding:8px 12px;
      font-size:14px;
    }
    .radx-accordion .accordion-body{
      padding:8px 12px;
    }
    .radx-instructions-text{
      margin:0;
      line-height:1.3;
    }
  "))
}
