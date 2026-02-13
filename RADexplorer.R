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

ui <- tagList(
  includeCSS("www/taxaSelect.css"),
  includeScript("www/taxaSelect.js"),
  uiOutput("page")
)

server <- function(input, output, session) {
  
  # this keeps track of which menu screen the user is on
  screen <- reactiveVal("menu")
  
  observeEvent(screen(), {
    req(screen() == "taxaSelect")
    
    session$onFlushed(function() {
      updateSelectizeInput(session, "selectGenus", selected = "", choices = genus, server = TRUE)
    }, once = TRUE)
  })
  
  observeEvent(input$selectGenus, {
    req(screen() == "taxaSelect")
    req(input$selectGenus)
    
    selected_genera <- input$selectGenus  
    
    line_genus <- sub("\\s.*$", "", genus_species)  
    sp <- genus_species[line_genus %in% selected_genera]
    
    updateSelectizeInput(
      session,
      "selectTaxa",
      choices = sp,
      selected = character(0),
      server = TRUE
    )
  })
  
  observeEvent(input$selectGenus, {
    req(screen() == "taxaSelect")
    
    selected_genera <- input$selectGenus
    n_genera <- if (is.null(selected_genera)) 0 else length(selected_genera)
    
    if (n_genera == 0) return()
    
    line_genus <- sub("\\s.*$", "", genus_species)
    sp <- genus_species[line_genus %in% selected_genera]
    n_members <- length(unique(sp))
    
    genus_word <- if (n_genera == 1) "genus" else "genera"
    label <- if (n_members == 1) {
      paste0("Analyze the only member of the selected ", genus_word)
    } else {
      paste0("Analyze all ", n_members, " members of the selected ", genus_word)
    }
    
    updateCheckboxInput(session, "entireGenus", label = label, value = FALSE)
  }, ignoreInit = TRUE)
  
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
            h4("Welcome to RADexplorer!"),
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
            id = "taxaCard",
            style = "width: min(1100px, 80vw); max-height: 90vh; overflow: visible;",
            card_body(
              style = "display:flex; flex-direction:column; gap:16px; overflow:auto;",
              selectizeInput(
                "selectGenus", "Select genus or genera to analyze:",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Type to search", maxOptions = 10000),
                width = "100%"
              ),
              conditionalPanel(
                condition = "input.selectGenus && input.selectGenus.length > 0",
                checkboxInput(
                  "entireGenus",
                  "",
                  TRUE,
                  width = "auto"
                )
              ),
              conditionalPanel(
                condition = "input.entireGenus == false",
                div(
                  style = "flex: 0 1 700px; width: 100%;",
                  selectizeInput(
                    "selectTaxa", "Select species to analyze:",
                    choices = NULL, multiple = TRUE,
                    options = list(
                      placeholder = "Type to search",
                      maxOptions = 10000,
                      closeAfterSelect = FALSE
                    ),
                    width = "100%"
                  )
                )
              ),
              div(
                style = "display:flex; gap:12px; width:100%;",
                actionButton("backToMenu", "Back", style = "flex:1;"),
                actionButton("continueWithTaxa", "Continue", style = "flex:1;")
              )
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