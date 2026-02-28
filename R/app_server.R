app_server <- function(input, output, session) {
  base_dir <- system.file("app", package = "RADexplorer")
  shiny::addResourcePath("app", base_dir)
  
  sys.source(file.path(base_dir, "visualization.R"), envir = environment())
  
  # these lines import the list of genus and species names for the dropdown menus
  genus <- readLines(file.path(base_dir, "taxa", "genus.txt"), warn = FALSE)
  genus <- trimws(genus)
  genus <- genus[nzchar(genus)]
  
  genus_species <- readLines(file.path(base_dir, "taxa", "Genusspecies.txt"), warn = FALSE)
  genus_species <- trimws(genus_species)
  genus_species <- genus_species[nzchar(genus_species)]
  
  library(shiny)
  library(bslib)
  library(plotly)
  library(shinyjs)
  
  # this keeps track of which menu screen the user is on
  screen <- reactiveVal("menu")
  
  # this keeps track of the user's selected taxa
  selected_taxa <- reactiveVal(NULL)
  
  # this puts the genus list into the drop down menu on the taxa select screen
  observeEvent(screen(), {
    req(screen() == "menu")
    
    session$onFlushed(function() {
      updateSelectizeInput(session, "selectGenus", selected = "", choices = genus, server = TRUE)
    }, once = TRUE)
  })
  
  # this puts the filtered species list into the drop down menu after the genus/genera are selected
  observeEvent(input$selectGenus, {
    req(screen() == "menu")
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
  
  # this is just a section that changes wording on the taxa select page based on how many genera / species you've selected
  # simply for aesthetics
  observeEvent(input$selectGenus, {
    req(screen() == "menu")
    
    selected_genera <- input$selectGenus
    n_genera <- if (is.null(selected_genera)) 0 else length(selected_genera)
    
    if (length(selected_genera) == 0) {
      updateCheckboxInput(
        session,
        "entireGenus",
        label = HTML("<i>Analyze all members of the selected genus/genera</i>"),
        value = FALSE
      )
      shinyjs::disable("entireGenus")
      return()
    }
    
    line_genus <- sub("\\s.*$", "", genus_species)
    sp <- genus_species[line_genus %in% selected_genera]
    n_members <- length(unique(sp))
    
    genus_word <- if (n_genera == 1) "genus" else "genera"
    label <- if (n_members == 1) {
      paste0("Analyze the only member of the selected ", genus_word)
    } else {
      paste0("Analyze all ", n_members, " members of the selected ", genus_word)
    }
    
    # sets the checkbox text, in italics
    updateCheckboxInput(
      session,
      "entireGenus",
      label = HTML(paste0("<i>", label, "</i>")),
      value = FALSE
    )
    
    # checks to see if its greater than the maximum. if so, disables the checkmark
    if (n_members > 15 || n_members == 0) {
      shinyjs::disable("entireGenus")
    } else {
      shinyjs::enable("entireGenus")
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(screen(), {
    shinyjs::disable("entireGenus")
    updateCheckboxInput(session, "entireGenus", value = FALSE)
  }, ignoreInit = FALSE)
  
  # selected number of species note under the speciesSelection checkbox
  output$speciesNote <- renderUI({
    selected_species <- input$selectTaxa
    n_selected <- if (is.null(selected_species)) 0 else length(selected_species)
    
    col <- if (n_selected >= 1 && n_selected <= 15) "green" else "red"
    
    HTML(paste0(
      "<p><i>Note: a maximum of 15 species can be selected for analysis. You have selected: ",
      "<span style='color:", col, "; font-weight:700;'>", n_selected, "</span>.",
      "</i></p>"
    ))
  })
  
  # this actually puts all of the species options into the species selectize so radexplorer can keep track of the selected species
  observeEvent(input$entireGenus, {
    
    selected_genera <- input$selectGenus
    line_genus <- sub("\\s.*$", "", genus_species)
    sp <- genus_species[line_genus %in% selected_genera]
    
    if (isTRUE(input$entireGenus)) {
      updateSelectizeInput(
        session,
        "selectTaxa",
        selected = sp,
        choices = unique(sp),
        server = TRUE
      )
    } else {
      updateSelectizeInput(
        session,
        "selectTaxa",
        selected = character(0),
        choices = unique(sp),
        server = TRUE
      )
    }
  })
  
  # this activates the explore + download buttons on the menu page when the user has selected the taxa of interest
  observe({
    ok <- length(input$selectGenus) > 0 && length(input$selectTaxa) > 0 && length(input$selectTaxa) <= 15
    shinyjs::toggleState("download", condition = ok)
    shinyjs::toggleState("continueWithTaxa", condition = ok)
  })
  
  # this sets the selectedTaxa variable with the users selections
  observeEvent(input$continueWithTaxa, {
    print(input$selectTaxa)
    print(as.list(input$selectTaxa))
    selected_taxa(input$selectTaxa)
    screen("radx")
  })
  
  # this changes the selected screen based on whether the user is using RADlib or their own database
  observeEvent(input$continue, {
    req(input$mode)
    if (input$mode == "RADlib") screen("menu")
    if (input$mode == "upload") screen("upload")
  })
  
  # this is the event that takes the user back to the main menu
  observeEvent(input$backToMenu, {
    screen("menu")
  })
  
  # this takes the user to radx
  observeEvent(input$continueWithTaxa, {
    screen("radx")
  })
  
  # this takes the user to the taxa select screen
  observeEvent(input$tomenu, {
    screen("menu")
  })
  
  # this is if the user wants to use their own database - i've left a placeholder here for the time being
  observeEvent(input$continueWithFile, {
    req(input$altlib)
    screen("placeholder")  
  })
  
  # this is the meat of the screen rendering
  # it selects the screen that the user is seeing based on the variable above and renders it accordingly
  output$page <- renderUI({
    if (screen() == "radx") {
      # rad explorer menu
      page_sidebar(
        sidebar = sidebar(
          h5("Options"),
          # variable region select
          checkboxGroupInput(
            "varRegions",
            "Select all 16S gene variable regions to include:",
            choices = setNames(paste0("V",1:9,"regions"), 1:9),
            selected = paste0("V",1:9,"regions")
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
      
    } else if (screen() == "menu") {
      # taxa select menu
      page_fillable(
        title = "RADexplorer",
        div(
          style = "display:flex; align-items:flex-start; justify-content:center; padding-top:100px; padding-bottom:100px;",
          card(
            id = "taxaCard",
            style = "width: min(1100px, 80vw); max-height: 250vh; overflow: visible;",
            card_body(
              style = "display:flex; flex-direction:column; gap:16px; overflow:auto;",
              div(
                style = "display:flex; gap:12px; width:100%;",
                h4("Welcome to RADexplorer!", style = "margin:0;"),
                #actionButton("backToMenu", "Back", style = "margin-left:auto;")
              ),
              # this is the Reference Library card
              # it currently grays out the option to select the library and forces the user to select RADlib
              card(
                div(
                  style = "display:flex; align-items:center; gap:10px;",
                  tags$label(`for` = "ref_lib", "Reference library:", style = "margin:0; white-space:nowrap;"),
                  tags$select(
                    id = "ref_lib",
                    class = "form-control",
                    disabled = "disabled",
                    # sets the width of the drop down button - a little neurotic, can be removed lol
                    style = "width: 160px; flex: 0 0 100px;",
                    tags$option(value = "RADlib", "RADlib v1.0", selected = "selected")
                  )
                )
              ),
              card(
                div(
                  style = "display:flex; flex-direction:column; gap:0px;",
                  # genus selection
                  tags$div(
                    tags$label(
                      `for` = "selectGenus",
                      style = "display:block; margin-bottom:0px;",
                      tags$div("Select genus or genera to analyze:"),
                      tags$div(
                        style = "font-size:13px; margin-top:0px;",
                        class = "checkbox",
                        tags$label(
                          checkboxInput(
                            "entireGenus",
                            label = HTML("<i>Analyze all members of the selected genus</i>"),
                            value = FALSE
                          )
                        )
                      )
                    ),
                    selectizeInput(
                      "selectGenus", label = NULL,
                      choices = NULL, multiple = TRUE,
                      options = list(placeholder = "Type to search", maxOptions = 10000),
                      width = "100%"
                    )
                  ),
                  # species select - only shows up after you select a genus or genera
                  conditionalPanel(
                    condition = "input.selectGenus && input.selectGenus.length > 0",
                    selectizeInput(
                      "selectTaxa", "Select species to analyze:",
                      choices = NULL, multiple = TRUE,
                      options = list(
                        placeholder = "Type to search",
                        maxOptions = 10000,
                        closeAfterSelect = FALSE
                      ),
                      width = "100%"
                    ),
                    div(
                      style = "font-size: 13px; margin-top:-6px;",
                      uiOutput("speciesNote")
                    )
                  )
                )
              ),
              
              fluidPage(
                useShinyjs(),
                div(
                  style = "display:flex; gap:12px; width:100%;",
                  actionButton("download", "Download", style = "flex:1;"),
                  actionButton("continueWithTaxa", "Explore", style = "flex:1;")
                )
              )
            )
          )
        )
      )
    }
  })
  
  
  msa_plot <- eventReactive(list(input$continueWithTaxa, input$varRegions, input$detailedView, input$uniqueRegions), {
    make_msa_plotly(
      taxon = selected_taxa(),
      varRegions = input$varRegions,
      highlight_unique = input$uniqueRegions
    )
  })
  
  # outputs the plot to the card
  output$visual <- renderPlotly({
    msa_plot()
  })
  
}
