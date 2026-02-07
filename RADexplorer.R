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
      choices = list("1" = 1,"2" = 2,"3" = 3,"4" = 4,"5" = 5,"6" = 6,"7" = 7,"8" = 8,"9" = 9),
      selected = c(3,4)
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

