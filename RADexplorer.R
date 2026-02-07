library(shiny)
library(bslib)
library(plotly)
source("visualization.R")

ui = page_sidebar(
  title = "RAD Explorer",
  sidebar = sidebar(
    title = "Navigate",
    textInput(
      "taxon",
      label = "Input taxon to explore:",
      value = NULL
    ),
    checkboxGroupInput(
      "varRegions",
      "Select all 16S gene variable regions to include:",
      choices = list("1" = "V1regions","2" = "V2regions","3" = "V3regions","4" = "V4regions","5" = "V5regions","6" = "V6regions","7" = "V7regions","8" = "V8regions","9" = "V9regions"),
      selected = c("V3regions","V4regions")
    ),
    
    actionButton("submit", "Submit")
  ),
  card(plotlyOutput("visual", height = "650px"))
)

server = function(input, output, session) {
  
  msa_plot <- eventReactive(input$submit, {
    make_msa_plotly(
      taxon = input$taxon,
      varRegions = input$varRegions
    )
  })
  
  output$visual <- renderPlotly({
    msa_plot()
  })
}



shinyApp(ui = ui, server = server)

