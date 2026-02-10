library(shiny)
library(bslib)
library(plotly)
source("visualization.R")

ui <- uiOutput("page")

server <- function(input, output, session) {
  
  output$page <- renderUI({
    # before user selects database to use
    if (is.null(input$continue) || input$continue == 0) {
      page_fixed(
        title = "RADexplorer",
        card(
          h5("Would you like to use RADlib or upload your own database?"),
          radioButtons(
            "mode",
            label = NULL,
            choices = c("RADlib" = "RADlib", "Upload my own" = "upload")
          ),
          actionButton("continue", "Continue")
        )
      )
    } else {
      # after user selects database to use
      req(input$mode == "RADlib")
      
      page_sidebar(
        title = "RADexplorer",
        sidebar = sidebar(
          title = "Navigate",
          textInput("taxon", "Input taxon to explore:", value = "test data"),
          checkboxGroupInput(
            "varRegions",
            "Select all 16S gene variable regions to include:",
            choices = list(
              "1" = "V1regions","2" = "V2regions","3" = "V3regions","4" = "V4regions",
              "5" = "V5regions","6" = "V6regions","7" = "V7regions","8" = "V8regions","9" = "V9regions"
            ),
            selected = c("V1regions","V2regions","V3regions","V4regions","V5regions",
                         "V6regions","V7regions","V8regions","V9regions")
          ),
          actionButton("submit", "Submit")
        ),
        card(
          conditionalPanel("input.submit == 0",
                           div("Select options in the sidebar, then click Submit.", style = "padding:12px;")
          ),
          conditionalPanel("input.submit > 0",
                           plotlyOutput("visual", height = "650px")
          )
        )
      )
    }
  })
  
  msa_plot <- eventReactive(input$submit, {
    req(input$mode == "RADlib")
    make_msa_plotly(taxon = input$taxon, varRegions = input$varRegions)
  })
  
  output$visual <- renderPlotly({
    req(input$submit > 0)
    msa_plot()
  })
}

shinyApp(ui, server)