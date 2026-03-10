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
      input_switch(
        "detailedView",
        label = "Detailed View",
        value = TRUE
      ),
      # unique region view button
      input_switch(
        "uniqueRegions",
        label = "Unique region view",
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
