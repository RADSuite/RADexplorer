radx_screen_ui <- function() {
  # rad explorer menu
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shiny::h5("Options"),
      # variable region select
      shiny::checkboxGroupInput(
        "varRegions",
        "Select all 16S rRNA gene v-regions for analysis:",
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
    bslib::card(
      p(
        "All 16S rRNA gene copies for the selected taxa are shown below. ",
        "Within each v-region column, color represents identical sequences. ",
        "Select v-regions (left sidebar) for analysis. ",
        "A green checkmark next to a taxon means it can be identified using the selected v-region(s). ",
        "Taxa within a red bracket cannot be distinguished from eachother with the selected v-region(s). "
      ),
      bslib::card(
        shiny::conditionalPanel(
          "input.continueWithTaxa > 0",
          plotly::plotlyOutput("visual", height = "650px")
        )
      )
    )
  )
}
