radx_screen_ui <- function() {
  # rad explorer menu
  page_sidebar(
    sidebar = sidebar(
      h5("Options"),
      # variable region select
      checkboxGroupInput(
        "varRegions",
        "Select all 16S gene variable regions to include:",
        choices = setNames(paste0("V",1:9), 1:9),
        selected = paste0("V",1:9)
      ),
      div(
        style = "margin-top:-16px; margin-bottom:8px; font-size:12px;",
        actionLink("deselectVarRegions", "Deselect all v-regions")
      ),
      input_switch(
        "detailedView",
        label = "Detailed View",
        value = TRUE
      ),
      input_switch(
        "vregionIDs",
        label = "Display V-Region IDs",
        value = FALSE
      ),
      div(
        style = "display:flex; gap:10px; width:100%;",
        actionButton("backToMenu", "Back", style = "flex:1;")
      )
    ),
    card(
      conditionalPanel("input.continueWithTaxa > 0",
                       plotlyOutput("visual", height = "650px")
      )
    )
  )
}
