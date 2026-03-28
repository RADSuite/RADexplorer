#' Shiny server for RADexplorer
#'
#' Defines the main server logic for the RADexplorer application.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return No return value.
#' @export
app_server <- function(input, output, session) {
  # package app path
  base_dir <- system.file("app", package = "RADexplorer")
  shiny::addResourcePath("app", base_dir)

  # dropdown support files
  genus <- readLines(file.path(base_dir, "taxa", "genus.txt"), warn = FALSE)
  genus <- trimws(genus)
  genus <- genus[nzchar(genus)]

  genus_species <- readLines(file.path(base_dir, "taxa", "Genusspecies.txt"), warn = FALSE)
  genus_species <- trimws(genus_species)
  genus_species <- genus_species[nzchar(genus_species)]

  # full taxa list
  all_organisms <- sort(unique(RADalign::get_all_organisms()))

  # current screen
  screen <- shiny::reactiveVal("menu")

  # selected taxa
  selected_taxa <- shiny::reactiveVal(character(0))
  selected_taxa_metascope_filter <- shiny::reactiveVal(character(0))

  # selected variable regions
  selected_vregions <- shiny::reactiveVal(c("V4"))

  # RADalign outputs
  RADq <- shiny::reactiveVal(NULL)
  uniqueRADq <- shiny::reactiveVal(NULL)
  RADqGroups <- shiny::reactiveVal(NULL)

  # selected download pipeline
  download_pipeline <- shiny::reactiveVal(NULL)

  # loading flag
  loading <- shiny::reactiveVal(FALSE)

  #########################################################################
  # helper functions

  # updates the italic label for whole-genus filtering
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

    shiny::updateCheckboxInput(
      session,
      checkbox_id,
      label = shiny::HTML(paste0("<i>", label, "</i>")),
      value = FALSE
    )
  }

  # toggles the metascope filter card on/off
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

  # reloads the taxa dropdown whenever user returns to the menu
  shiny::observeEvent(screen(), {
    shiny::req(screen() == "menu")

    shiny::updateSelectizeInput(
      session,
      "selectTaxa",
      choices = all_organisms,
      selected = selected_taxa(),
      server = TRUE
    )
  }, ignoreInit = FALSE)

  #########################################################################
  # main taxa selection screen

  # note under the taxa selector
  output$speciesNote <- shiny::renderUI({
    n_selected <- length(input$selectTaxa %||% character(0))

    shiny::HTML(paste0(
      "<p><i>You have selected ",
      n_selected,
      " taxa.",
      "</i></p>"
    ))
  })

  # stores selected taxa
  shiny::observeEvent(input$selectTaxa, {
    selected_taxa(input$selectTaxa %||% character(0))
  }, ignoreInit = TRUE)

  # stores metascope filter taxa
  shiny::observeEvent(input$selectTaxaFilter, {
    selected_taxa_metascope_filter(input$selectTaxaFilter %||% character(0))
  }, ignoreInit = TRUE)

  # enables buttons only when taxa are selected
  shiny::observe({
    ok <- length(input$selectTaxa %||% character(0)) > 0
    shinyjs::toggleState("download", condition = ok)
    shinyjs::toggleState("continueWithTaxa", condition = ok)
  })

  #########################################################################
  # RADx flow

  # sends selected taxa to RADalign and opens the explorer
  shiny::observeEvent(input$continueWithTaxa, {
    taxa_now <- isolate(selected_taxa())
    vregions_now <- isolate(selected_vregions())

    loading(TRUE)
    screen("loading")

    later::later(function() {
      RADq(RADalign::createRADq(taxa_now, TRUE))
      uniqueRADq(RADalign::createSummarizedIDs(TRUE))
      RADqGroups(RADalign::createRADqGroups(vregions_now, TRUE))

      loading(FALSE)
      screen("radx")
    }, delay = 0)
  })

  # updates selected variable regions
  shiny::observeEvent(input$varRegions, {
    selected_vregions(input$varRegions)
    RADqGroups(RADalign::createRADqGroups(selected_vregions(), TRUE))
    print(RADqGroups())

    ########## THIS IS WHERE WE SEND THE SELECTED TAXA TO RADALIGN AND RECIEVE RADq ############
    # RADq(RADalign::selectVRegions(selected_vregions(), TRUE))
    ##########                                                                      ############
  })

  # deselects all variable regions
  shiny::observeEvent(input$deselectVarRegions, {
    shiny::updateCheckboxGroupInput(
      session,
      "varRegions",
      selected = character(0)
    )
  })

  #########################################################################
  # screen navigation

  # back to main menu
  shiny::observeEvent(input$backToMenu, {
    screen("menu")
  })

  # opens download screen
  shiny::observeEvent(input$download, {
    screen("RADport")
  })

  # opens metascope download flow
  shiny::observeEvent(input$continueMetascope, {
    download_pipeline("metascope")
    screen("metascope")
  })

  # back from instructions to metascope page
  shiny::observeEvent(input$backToMetascopeDownload, {
    screen("metascope")
  })

  # opens metascope instructions
  shiny::observeEvent(input$metascopeInstructionsButton, {
    screen("metascopeInstructions")
  })

  #########################################################################
  # MetaScope screen logic

  # enables/disables the filter card
  shiny::observe({
    set_metascope_filter_card_state(isTRUE(input$useMetascopeFilters))
  })

  # updates the whole-genus filter label
  shiny::observeEvent(input$selectGenusFilter, {
    selected_genera_filter <- input$selectGenusFilter
    n_genera <- length(selected_genera_filter %||% character(0))

    if (n_genera == 0) {
      shiny::updateCheckboxInput(
        session,
        "entireGenusFilter",
        label = shiny::HTML("<i>Filter all members of the selected genus/genera</i>"),
        value = FALSE
      )
      return()
    }

    sp <- get_species_from_genera(selected_genera_filter)

    update_entire_genus_label(
      checkbox_id = "entireGenusFilter",
      n_genera = n_genera,
      n_members = length(sp),
      mode = "filter"
    )
  }, ignoreInit = TRUE)

  # fills species filter with all members of selected genera
  shiny::observeEvent(input$entireGenusFilter, {
    shiny::req(input$selectGenusFilter)

    sp <- get_species_from_genera(input$selectGenusFilter)

    if (isTRUE(input$entireGenusFilter)) {
      shiny::updateSelectizeInput(
        session,
        "selectTaxaFilter",
        selected = sp,
        server = TRUE
      )
    }
  }, ignoreInit = TRUE)

  # exports selected RAD databases
  shiny::observeEvent(input$port, {
    ########## THIS IS WHERE WE DOWNLOAD THE FILES FOR PORTING TO OTHER PIPELINES ############
    if (download_pipeline() == "metascope") {
      if (length(input$selectTaxaFilter %||% character(0)) != 0) {
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

  # chooses which page to show
  output$page <- shiny::renderUI({
    if (screen() == "radx") {
      radx_screen_ui()
    } else if (screen() == "menu") {
      menu_screen_ui()
    } else if (screen() == "loading") {
      loading_screen_ui()
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

  # rebuilds the plot when relevant controls change
  msa_plot <- shiny::eventReactive(
    list(input$continueWithTaxa, input$varRegions, input$detailedView, input$vregionIDs),
    {
      shiny::req(RADq(), uniqueRADq(), RADqGroups())

      print(RADq())

      make_msa_plotly(
        RADq = RADq(),
        unique = uniqueRADq(),
        groups = RADqGroups(),
        varRegions = selected_vregions(),
        detailed = input$detailedView,
        vregionIDs = input$vregionIDs
      )
    }
  )

  # sends the plotly object to the UI
  output$visual <- plotly::renderPlotly({
    msa_plot()
  })
}
