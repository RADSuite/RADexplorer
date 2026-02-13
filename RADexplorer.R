library(shiny)
library(bslib)
library(plotly)
source("visualization.R")

genus <- readLines("testdata/genus.txt", warn = FALSE)
genus <- trimws(genus)
genus <- genus[nzchar(genus)]

genus_species <- readLines("testdata/Genusspecies.txt", warn = FALSE)
genus_species <- trimws(genus_species)
genus_species <- genus_species[nzchar(genus_species)]

ui <- uiOutput("page")

server <- function(input, output, session) {
  
  # this keeps track of which menu screen the user is on
  screen <- reactiveVal("menu")
  
  observeEvent(screen(), {
    req(screen() == "taxaSelect")
    
    session$onFlushed(function() {
      updateSelectizeInput(session, "selectGenus", choices = genus, server = TRUE)
    }, once = TRUE)
  })
  
  observeEvent(input$selectGenus, {
    req(screen() == "taxaSelect")
    req(input$selectGenus)
    
    prefix <- paste0(input$selectGenus, " ")
    sp <- genus_species[startsWith(genus_species, prefix)]
    
    updateSelectizeInput(
      session,
      "selectTaxa",
      choices = sp,
      selected = character(0),
      server = TRUE
    )
  })
  
  observeEvent(input$continue, {
    req(input$mode)
    if (input$mode == "RADlib") screen("taxaSelect")
    if (input$mode == "upload") screen("upload")
  })
  
  
  observeEvent(input$backToMenu, {
    screen("menu")
  })
  
  observeEvent(input$continueWithTaxa, {
    screen("radlib")
  })
  
  observeEvent(input$toTaxaSelect, {
    screen("taxaSelect")
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
            h4("Welcome to RADexplorer!", style = "text-decoration: underline;"),
            p("Would you like to use native RADlib or upload your own database?"),
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
        title = "RADx",
        sidebar = sidebar(
          textInput("taxon", "Input taxon to explore:", value = "test data"),
          checkboxGroupInput(
            "varRegions",
            "Select all 16S gene variable regions to include:",
            choices = setNames(paste0("V",1:9,"regions"), 1:9),
            selected = paste0("V",1:9,"regions")
          ),
          checkboxInput(
            "uniqueRegions",
            label = "Unique region view",
            value = FALSE
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
    } else if (screen() == "taxaSelect") {
      page_fillable(
        title = "Select taxa to analyze",
        div(
          style = "height: calc(100vh - 80px); display:flex; align-items:center; justify-content:center;",
          card(
            style = "width: min(1200px, 96vw); height: min(900px, 92vh);",
            card_body(
              style = "height: 90%; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:16px;",
              
              selectizeInput(
                "selectGenus",
                "Select genus below:",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Type to search", maxOptions = 200)
              ),
              
              checkboxInput("entireGenus", "Analyze entire genus?", TRUE),
              
              conditionalPanel(
                condition = "input.entireGenus == false",
                selectizeInput(
                  "selectTaxa",
                  "Select species below:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(placeholder = "Type to search", maxOptions = 200,
                                 closeAfterSelect = FALSE)
                )
              ),
              actionButton("continueWithTaxa", "Explore")
            )
          )
        )
      )
    }
  })
  
  msa_plot <- eventReactive(input$submit, {
    req(input$mode == "RADlib")
    make_msa_plotly(taxon = input$taxon, varRegions = input$varRegions, highlight_unique = input$uniqueRegions)
  })
  
  output$visual <- renderPlotly({
    req(input$submit > 0)
    msa_plot()
  })
}

shinyApp(ui, server)