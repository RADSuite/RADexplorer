library(shiny)
library(bslib)
library(plotly)
source("visualization.R")

ui <- uiOutput("page")

server <- function(input, output, session) {
  
  # this keeps track of which menu screen the user is on
  screen <- reactiveVal("menu")
  
  observeEvent(input$continue, {
    req(input$mode)
    if (input$mode == "RADlib") screen("radlib")
    if (input$mode == "upload") screen("upload")
  })
  
  observeEvent(input$backToMenu, {
    screen("menu")
  })
  
  observeEvent(input$continueWithFile, {
    req(input$altlib)
    screen("placeholder")  
  })
  
  output$page <- renderUI({
    if (screen() == "menu") {
      page_fillable(
        title = "RADexplorer",
        div(
          style = "height: calc(100vh - 80px); display:flex; align-items:center; justify-content:center;",
          card(
            h5("Would you like to use RADlib or upload your own database?"),
            radioButtons(
              "mode", label = NULL,
              choices = c("RADlib" = "RADlib", "Upload my own" = "upload")
            ),
            actionButton("continue", "Continue")
          )
        )
      )
      
    } else if (screen() == "radlib") {
      page_sidebar(
        title = "RADexplorer",
        sidebar = sidebar(
          textInput("taxon", "Input taxon to explore:", value = "test data"),
          checkboxGroupInput(
            "varRegions",
            "Select all 16S gene variable regions to include:",
            choices = setNames(paste0("V",1:9,"regions"), 1:9),
            selected = paste0("V",1:9,"regions")
          ),
          div(
            style = "display:flex; gap:10px; width:100%;",
            actionButton("submit", "Submit", style = "flex:1;"),
            actionButton("backToMenu", "Back", style = "flex:1;")
          )
        ),
        card(
          conditionalPanel("input.submit == 0",
                           div("Select options in the sidebar, then click Submit.", style="padding:12px;")
          ),
          conditionalPanel("input.submit > 0",
                           plotlyOutput("visual", height = "650px")
          )
        )
      )
      
    } else if (screen() == "upload") {
      page_fillable(
        title = "Upload database file",
        div(
          style = "height: calc(100vh - 80px); display:flex; align-items:center; justify-content:center;",
          card(
            h5("Upload file below"),
            fileInput("altlib", label = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
            div(
              style = "display:flex; gap:12px; width:100%;",
              actionButton("backToMenu", "Back", style = "flex:1;"),
              actionButton("continueWithFile", "Continue", style = "flex:1;")
            )
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