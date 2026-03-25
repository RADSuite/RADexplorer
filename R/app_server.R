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
  selected_taxa_metascope_filter <- reactiveVal(NULL)

  # this keeps track of the user's selected variable regions
  selected_vregions <- reactiveVal(c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9"))
  vregionIDs = reactiveVal(FALSE)

  # this keeps track of the RADq dataframe that is returned from RADalign
  RADq <- reactiveVal(NULL)
  uniqueRADq <- reactiveVal(NULL)
  uniqueVregions <- reactiveVal(NULL)
  RADqGroups <- reactiveVal(NULL)
  loaded <- reactiveVal(FALSE)

  # selected download pipeline
  download_pipeline <- reactiveVal(NULL)

  #########################################################################
  # helper functions

  get_species_from_genera <- function(selected_genera_vector) {
    if (is.null(selected_genera_vector) || length(selected_genera_vector) == 0) {
      return(character(0))
    }

    line_genus <- sub("\\s.*$", "", genus_species)
    unique(genus_species[line_genus %in% selected_genera_vector])
  }

  update_taxa_selectize <- function(input_id, choices, selected = character(0)) {
    updateSelectizeInput(
      session,
      input_id,
      choices = choices,
      selected = selected,
      server = TRUE
    )
  }

  update_entire_genus_label <- function(checkbox_id, n_genera, n_members, mode = c("analyze", "filter")) {
    mode <- match.arg(mode)

    genus_word <- if (n_genera == 1) "genus" else "genera"

    label <- if (mode == "analyze") {
      if (n_members == 1) {
        paste0("Analyze the only member of the selected ", genus_word)
      } else {
        paste0("Analyze all ", n_members, " members of the selected ", genus_word)
      }
    } else {
      if (n_members == 1) {
        paste0("Filter the only member of the selected ", genus_word)
      } else {
        paste0("Filter all ", n_members, " members of the selected ", genus_word)
      }
    }

    updateCheckboxInput(
      session,
      checkbox_id,
      label = HTML(paste0("<i>", label, "</i>")),
      value = FALSE
    )
  }

  set_metascope_filter_card_state <- function(enabled) {
    shinyjs::toggleState("selectGenusFilter", condition = enabled)
    shinyjs::toggleState("entireGenusFilter", condition = enabled)
    shinyjs::toggleState("selectTaxaFilter", condition = enabled)

    if (enabled) {
      shinyjs::removeClass("metascopeFilterCard", "text-muted")
      shinyjs::runjs("
        var el = document.getElementById('metascopeFilterCard');
        if (el) {
          el.style.opacity = '1';
          el.style.backgroundColor = '';
        }
      ")
    } else {
      shinyjs::addClass("metascopeFilterCard", "text-muted")
      shinyjs::runjs("
        var el = document.getElementById('metascopeFilterCard');
        if (el) {
          el.style.opacity = '0.5';
          el.style.backgroundColor = '#f8f9fa';
        }
      ")
    }
  }

  #########################################################################
  # menu screen setup

  observeEvent(screen(), {
    req(screen() == "menu")

    if (is.null(selected_taxa())) {
      session$onFlushed(function() {
        updateSelectizeInput(
          session,
          "selectTaxa",
          selected = character(0),
          choices = RADalign::get_all_organisms(),
          server = TRUE
        )
      }, once = TRUE)
    }
  })

  #########################################################################
  # main taxa selection screen

  # selected number of species note under the speciesSelection checkbox
  output$speciesNote <- renderUI({
    selected_species <- input$selectTaxa
    n_selected <- if (is.null(selected_species)) 0 else length(selected_species)

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

  # this activates the explore + download buttons on the menu page when the user has selected the taxa of interest
  observe({
    ok <- length(input$selectTaxa) > 0 &&
      length(input$selectTaxa) <= 15

    shinyjs::toggleState("download", condition = ok)
    shinyjs::toggleState("continueWithTaxa", condition = ok)
  })

  #########################################################################
  # RADx flow

  # this takes the user to RADx
  # and sets the selectedTaxa variable with the users selections
  # and recieves RADq from RADalign
  observeEvent(input$continueWithTaxa, {
    selected_taxa(input$selectTaxa)

    print(selected_taxa())
    ########## THIS IS WHERE WE SEND THE SELECTED TAXA TO RADALIGN AND RECIEVE RADq ############
    RADq(RADalign::createRADq(selected_taxa(), TRUE))
    uniqueRADq(RADalign::createSummarizedIDs(TRUE))
    RADqGroups(RADalign::createRADqGroups(selected_vregions(), TRUE))
    uniqueVregions(RADalign::createUniqueVregions(TRUE))
    ##########                                                                      ############
    #print(utils::head(RADq()))
    #print(class(uniqueRADq()))
    #print(uniqueRADq())
    #print(RADqGroups())

    screen("radx")
  })

  # this sets the selected_vregions variable with the users selections
  observeEvent(input$varRegions, {
    selected_vregions(input$varRegions)
    RADqGroups(RADalign::createRADqGroups(selected_vregions(), TRUE))
    print(RADqGroups())


    ########## THIS IS WHERE WE SEND THE SELECTED TAXA TO RADALIGN AND RECIEVE RADq ############
    #RADq(RADalign::selectVRegions(selected_vregions(), TRUE))
    ##########                                                                      ############
  })

  # Deselect all button
  observeEvent(input$deselectVarRegions, {
    updateCheckboxGroupInput(
      session,
      "varRegions",
      selected = character(0)
    )
  })

  #########################################################################
  # screen navigation

  # this is the event that takes the user back to the main menu
  observeEvent(input$backToMenu, {
    screen("menu")
  })

  # this takes the user to radport
  observeEvent(input$download, {
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

  # MetaScope instructions button goes to the instructions screen
  observeEvent(input$metascopeInstructionsButton, {
    screen("metascopeInstructions")
  })

  #########################################################################
  # MetaScope screen logic

  observe({
    enabled <- isTRUE(input$useMetascopeFilters)
    set_metascope_filter_card_state(enabled)
  })

  observeEvent(input$selectGenusFilter, {
    selected_genera_filter <- input$selectGenusFilter
    n_genera <- if (is.null(selected_genera_filter)) 0 else length(selected_genera_filter)

    if (n_genera == 0) {
      updateCheckboxInput(
        session,
        "entireGenusFilter",
        label = HTML("<i>Filter all members of the selected genus/genera</i>"),
        value = FALSE
      )

      update_taxa_selectize("selectTaxaFilter", character(0), character(0))
      return()
    }

    sp <- RADalign::get_all_organisms()
    n_members <- length(sp)

    update_taxa_selectize("selectTaxaFilter", sp, character(0))
    update_entire_genus_label("entireGenusFilter", n_genera, n_members, mode = "filter")
  }, ignoreInit = TRUE)

  # this actually puts all of the species options into the MetaScope species selectize
  # so the app can keep track of the selected species there too
  observeEvent(input$entireGenusFilter, {
    req(input$selectGenusFilter)

    sp <- get_species_from_genera(input$selectGenusFilter)
  })

  observeEvent(input$port, {
    ########## THIS IS WHERE WE DOWNLOAD THE FILES FOR PORTING TO OTHER PIPELINES ############
    if (download_pipeline() == "metascope") {
      if (length(input$selectTaxaFilter) != 0) {
        RADalign::download_RAD_data("MetaScope", selected_taxa(), selected_taxa_metascope_filter())
      } else {
        RADalign::download_RAD_data("MetaScope", selected_taxa())
      }
    } else if (download_pipeline() == "kraken") {

    } else if (download_pipeline() == "qiime2") {

    }
    ##########                                                                    ############
  })

  #########################################################################
  # screen rendering

  # this is the meat of the screen rendering
  # it selects the screen that the user is seeing based on the variable above and renders it accordingly
  output$page <- renderUI({
    if (screen() == "radx") {
      radx_screen_ui()
    } else if (screen() == "menu") {
      menu_screen_ui()
    } else if (screen() == "RADport") {
      radport_screen_ui()
    } else if (screen() == "metascope") {
      metascope_screen_ui(genus, genus_species)
    } else if (screen() == "metascopeInstructions") {
      metascope_instructions_ui()
    }
  })

  #########################################################################
  # plot rendering

  msa_plot <- eventReactive(list(input$continueWithTaxa, input$varRegions, input$detailedView, input$vregionIDs), {
    print(RADq())
    #make_msa_plotly(
    #  RADq = RADq(),
    #  unique = uniqueRADq(),
    #  groups = RADqGroups(),
    #  uniqueVregions = uniqueVregions(),
    #  varRegions = selected_vregions(),
    #  detailed = input$detailedView,
    #  vregionIDs = input$vregionIDs
    #)
  })

  # outputs the plot to the card
  output$visual <- renderPlotly({
    msa_plot()
  })
}
