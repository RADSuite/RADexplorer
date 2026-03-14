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

  # this keeps track of the user's selected genera and taxa
  selected_genera <- reactiveVal(NULL)
  selected_taxa <- reactiveVal(NULL)
  selected_taxa_metascope_filter <- reactiveVal(NULL)

  # this keeps track of the user's selected variable regions
  selected_vregions <- reactiveVal(c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9"))

  # this keeps track of the RADq dataframe that is returned from RADalign
  RADq <- reactiveVal(NULL)
  uniqueRADq <- reactiveVal(NULL)
  loaded <- reactiveVal(FALSE)

  # selected download pipeline
  download_pipeline <- reactiveVal(NULL)

  # this puts the genus list into the drop down menu on the taxa select screen
  observeEvent(screen(), {
    req(screen() == "menu")

    shinyjs::disable("entireGenus")
    updateCheckboxInput(session, "entireGenus", value = FALSE)

    if (is.null(selected_taxa())) {
      session$onFlushed(function() {
        updateSelectizeInput(session, "selectGenus", selected = "", choices = genus, server = TRUE)
      }, once = TRUE)
    }
  })

  # this puts the filtered species list into the drop down menu after the genus/genera are selected
  # this is just a section that changes wording on the taxa select page based on how many genera / species you've selected
  # simply for aesthetics
  observeEvent(input$selectGenus, {
    selected_genera(input$selectGenus)
    selected_genera_local <- input$selectGenus
    n_genera <- if (is.null(selected_genera_local)) 0 else length(selected_genera_local)

    if (length(selected_genera_local) == 0) {
      updateCheckboxInput(
        session,
        "entireGenus",
        label = HTML("<i>Analyze all members of the selected genus/genera</i>"),
        value = FALSE
      )
      shinyjs::disable("entireGenus")

      updateSelectizeInput(
        session,
        "selectTaxa",
        choices = character(0),
        selected = character(0),
        server = TRUE
      )
      return()
    }

    line_genus <- sub("\\s.*$", "", genus_species)
    sp <- genus_species[line_genus %in% selected_genera_local]
    n_members <- length(unique(sp))

    updateSelectizeInput(
      session,
      "selectTaxa",
      choices = unique(sp),
      selected = character(0),
      server = TRUE
    )

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
  }, ignoreInit = TRUE)

  # selected number of species note under the speciesSelection checkbox
  output$speciesNote <- renderUI({
    selected_species <- input$selectTaxa
    n_selected <- if (is.null(selected_species)) 0 else length(selected_species)

    col <- if (n_selected >= 1 && n_selected <= 15) "green" else "red"

    HTML(paste0(
      "<p><i>You have selected ",
      n_selected,
      " taxa. ",
      "</i></p>"
    ))
  })

  observeEvent(input$selectTaxa, {
    selected_taxa(input$selectTaxa)
  }, ignoreInit = TRUE)

  observeEvent(input$selectTaxaFilter, {
    selected_taxa_metascope_filter(input$selectTaxaFilter)
  }, ignoreInit = TRUE)

  # this actually puts all of the species options into the species selectize so radexplorer can keep track of the selected species
  observeEvent(input$entireGenus, {
    req(screen() == "menu")
    req(input$selectGenus)

    line_genus <- sub("\\s.*$", "", genus_species)
    sp <- genus_species[line_genus %in% input$selectGenus]

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

  # this takes the user to RADx
  # and sets the selectedTaxa variable with the users selections
  # and recieves RADq from RADalign
  observeEvent(input$continueWithTaxa, {
    selected_genera(input$selectGenus)
    selected_taxa(input$selectTaxa)

    print(selected_taxa())
    ########## THIS IS WHERE WE SEND THE SELECTED TAXA TO RADALIGN AND RECIEVE RADq ############
    RADq(RADalign::createRADq(selected_taxa(), TRUE))
    uniqueRADq(RADalign::createSummarizedIDs(TRUE))
    ##########                                                                      ############
    print(utils::head(RADq()))
    print(class(uniqueRADq()))
    print(uniqueRADq())

    screen("radx")
  })

  # this sets the selected_vregions variable with the users selections
  observeEvent(input$varRegions, {
    selected_vregions(input$varRegions)

    ########## THIS IS WHERE WE SEND THE SELECTED TAXA TO RADALIGN AND RECIEVE RADq ############
    #RADq(RADalign::selectVRegions(selected_vregions(), TRUE))
    ##########                                                                      ############
  })

  # this is the event that takes the user back to the main menu
  observeEvent(input$backToMenu, {
    screen("menu")
  })

  # this takes the user to radport
  observeEvent(input$download, {
    selected_genera(input$selectGenus)
    selected_taxa(input$selectTaxa)
    screen("RADport")
  })

  # if user selects metascope on download screen
  observeEvent(input$continueMetascope, {
    download_pipeline("metascope")
    screen("metascope")
  })

  # back button from metascope instructions page
  observeEvent(input$backToMetascopeDownload, {
    screen("metascope")
  })

  observeEvent(input$port, {
    ########## THIS IS WHERE WE DOWNLOAD THE FILES FOR PORTING TO OTHER PIPELINES ############
    if (download_pipeline() == "metascope") {
      if (length(input$selectTaxaFilter) != 0) {
        RADalign::download_RAD_data("MetaScope", selected_taxa(), selected_taxa_metascope_filter())
      } else {
        RADalign::download_RAD_data("MetaScope", selected_taxa())
      }
      screen("metascopeInstructions")
    } else if (download_pipeline() == "kraken") {

    } else if (download_pipeline() == "qiime2") {

    }
    ##########                                                                    ############
  })

  # this is the meat of the screen rendering
  # it selects the screen that the user is seeing based on the variable above and renders it accordingly
  output$page <- renderUI({
    if (screen() == "radx") {
      radx_screen_ui()
    } else if (screen() == "menu") {
      menu_screen_ui(genus)
    } else if (screen() == "RADport") {
      radport_screen_ui()
    } else if (screen() == "metascope") {
      metascope_screen_ui(genus, genus_species)
    } else if (screen() == "metascopeInstructions") {
      metascope_instructions_ui()
    }
  })

  msa_plot <- eventReactive(list(input$continueWithTaxa, input$varRegions, input$detailedView), {
    make_msa_plotly(
      RADq = RADq(),
      unique = uniqueRADq(),
      varRegions = selected_vregions(),
      detailed = input$detailedView
    )
  })

  # outputs the plot to the card
  output$visual <- renderPlotly({
    msa_plot()
  })
}
