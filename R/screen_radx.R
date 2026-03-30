radx_screen_ui <- function() {
  # rad explorer menu
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shiny::h5("Options"),
      # variable region select
      shiny::checkboxGroupInput(
        "varRegions",
        "Select v-regions:",
        choices = paste0("V", 1:9),
        selected = c("V4")
      ),
      shiny::div(
        style = "margin-top:-16px; margin-bottom:8px; font-size:12px;",
        shiny::actionLink("deselectVarRegions", "Deselect all v-regions")
      ),
      bslib::input_switch(
        "detailedView",
        label = "Detailed View",
        value = TRUE
      ),
      bslib::input_switch(
        "vregionIDs",
        label = "Display V-Region Labels",
        value = FALSE
      ),
      shiny::div(
        style = "display:flex; gap:10px; width:100%;",
        shiny::actionButton("backToMenu", "Back", style = "flex:1;")
      )
    ),
    tags$style(HTML("
      .radx-visual-wrap {
        position: relative;
        min-height: 650px;
      }

      .radx-visual-output {
        transition: opacity 0.15s ease;
      }

      .radx-visual-loader {
        display: none;
        position: absolute;
        inset: 0;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        gap: 16px;
        background: rgba(255, 255, 255, 0.9);
        z-index: 10;
        text-align: center;
      }

      .radx-visual-wrap:has(.radx-visual-output .recalculating) .radx-visual-loader {
        display: flex;
      }

      .radx-visual-wrap:has(.radx-visual-output .recalculating) .radx-visual-output {
        opacity: 0;
      }

      .radx-spinner {
        width: 48px;
        height: 48px;
        border: 5px solid #d9d9d9;
        border-top: 5px solid #2c7c31;
        border-radius: 50%;
        animation: radx-spin 0.8s linear infinite;
      }

      @keyframes radx-spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      .radx-loader-text {
        font-size: 15px;
        color: #444;
      }
    ")),
    bslib::accordion(
      id = "radx_instructions",
      open = TRUE,
      bslib::accordion_panel(
        "RADx Instructions",
        p(
          "Select the 16S rRNA gene variable regions you wish to study in the left sidebar.",
          tags$br(),
          "A green checkmark (",
          span("✓", style = "color: green; font-weight: bold;"),
          ") indicates that a taxon can be identified using the selected v-region(s). ",
          tags$br(),
          "A red bracket (",
          span("[", style = "color: red; font-weight: bold;"),
          ") groups taxa that cannot be distinguished from one another with the selected v-region(s). ",
          tags$small(
            style = "display:block; font-style:italic; margin-top:8px; margin-bottom:12px;",
            "Note: Colors designate identical sequences within a v-region. Colors should not be compared across columns."
          )
        )
      )
    ),
    bslib::card(
      shiny::conditionalPanel(
        "input.continueWithTaxa > 0",
        radx_loading_plot("visual", height = "650px")
      )
    )
  )
}

radx_loading_plot <- function(id, height = "650px") {
  shiny::div(
    class = "radx-visual-wrap",
    shiny::div(
      class = "radx-visual-output",
      plotly::plotlyOutput(id, height = height)
    ),
    shiny::div(
      class = "radx-visual-loader",
      shiny::div(class = "radx-spinner"),
      shiny::div(class = "radx-loader-text", "Loading visualization...")
    )
  )
}


