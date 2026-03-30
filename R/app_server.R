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

  # app state
  screen <- shiny::reactiveVal("menu")
  selected_taxa <- shiny::reactiveVal(character(0))
  selected_taxa_metascope_filter <- shiny::reactiveVal(character(0))
  selected_vregions <- shiny::reactiveVal(c("V4"))
  RADq <- shiny::reactiveVal(NULL)
  uniqueRADq <- shiny::reactiveVal(NULL)
  RADqGroups <- shiny::reactiveVal(NULL)
  download_pipeline <- shiny::reactiveVal(NULL)
  loading <- shiny::reactiveVal(FALSE)

  # genus-wide dropdown pattern
  genus_all_species_pattern <- " - All Species(?: \\([0-9]+\\))?$"

  # helper to detect genus-wide dropdown options
  is_genus_all_species <- function(x) {
    grepl(genus_all_species_pattern, x)
  }

  # helper to get all species from selected genera
  get_species_from_genera <- function(selected_genera) {
    selected_genera <- selected_genera %||% character(0)

    if (length(selected_genera) == 0) {
      return(character(0))
    }

    genus_lookup <- sub(" .*$", "", genus_species)
    sort(genus_species[genus_lookup %in% selected_genera])
  }

  # expands any "[Genus] - All Species" selections to their species
  expand_selected_taxa <- function(taxa) {
    taxa <- taxa %||% character(0)

    if (length(taxa) == 0) {
      return(character(0))
    }

    genus_labels <- taxa[is_genus_all_species(taxa)]
    direct_taxa <- taxa[!is_genus_all_species(taxa)]

    genus_taxa <- unlist(
      lapply(genus_labels, RADalign::get_species_from_genus),
      use.names = FALSE
    )

    sort(unique(c(direct_taxa, genus_taxa)))
  }

  # rebuild taxa picker when entering menu
  shiny::observeEvent(screen(), {
    shiny::req(screen() == "menu")

    organisms <- sort(unique(RADalign::get_all_organisms()))

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectTaxa",
      choices = organisms,
      choicesOpt = list(
        style = ifelse(is_genus_all_species(organisms), "font-weight:700;", "")
      ),
      selected = selected_taxa()
    )
  }, ignoreInit = FALSE)

  # note under the taxa selector
  output$speciesNote <- shiny::renderUI({
    n_selected <- length(expand_selected_taxa(selected_taxa()))
    estimated_time <- n_selected + 6

    time_value <- if (estimated_time >= 60) {
      round(estimated_time / 60, 2)
    } else if (n_selected > 0) {
      estimated_time
    } else {
      0
    }

    time_unit <- if (estimated_time < 60) " seconds." else " minutes."

    tags$div(
      style = "margin:0; padding:0;",
      tags$i(
        paste0(
          "You have selected ", n_selected,
          " taxa. Estimated processing time: ",
          time_value,
          time_unit
        )
      )
    )
  })

  # store selected taxa from main picker
  shiny::observeEvent(input$selectTaxa, {
    selected_taxa(input$selectTaxa %||% character(0))
  }, ignoreInit = TRUE)

  # enable menu buttons only when taxa are selected
  shiny::observe({
    has_taxa <- length(selected_taxa()) > 0
    shinyjs::toggleState("download", condition = has_taxa)
    shinyjs::toggleState("continueWithTaxa", condition = has_taxa)
  })

  # run RADalign pipeline and move to analysis screen
  shiny::observeEvent(input$continueWithTaxa, {
    taxa_now <- isolate(expand_selected_taxa(selected_taxa()))
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

  # update selected variable regions
  shiny::observeEvent(input$varRegions, {
    shiny::req(RADq(), uniqueRADq())

    vr <- input$varRegions %||% character(0)
    selected_vregions(vr)

    if (length(vr) == 0) {
      RADqGroups(RADalign::createRADqGroups(paste0("V", 1:9), TRUE))
      return()
    }

    new_groups <- tryCatch(
      RADalign::createRADqGroups(vr, TRUE),
      error = function(e) NULL
    )

    if (!is.null(new_groups) && nrow(new_groups) > 0) {
      RADqGroups(new_groups)
    }
  }, ignoreNULL = FALSE)

  # clear selected variable regions
  shiny::observeEvent(input$deselectVarRegions, {
    shiny::updateCheckboxGroupInput(
      session,
      "varRegions",
      selected = character(0)
    )
  })

  # navigation between screens
  shiny::observeEvent(input$backToMenu, {
    screen("menu")
  })

  shiny::observeEvent(input$download, {
    screen("RADport")
  })

  shiny::observeEvent(input$continueMetascope, {
    download_pipeline("metascope")
    screen("metascope")
  })

  shiny::observeEvent(input$backToMetascopeDownload, {
    screen("metascope")
  })

  shiny::observeEvent(input$metascopeInstructionsButton, {
    screen("metascopeInstructions")
  })

  # export selected RAD databases
  shiny::observeEvent(input$port, {
    if (download_pipeline() == "metascope") {
      if (length(input$selectTaxaFilter %||% character(0)) != 0) {
        RADalign::download_RAD_data(
          "MetaScope",
          selected_taxa(),
          selected_taxa_metascope_filter()
        )
      } else {
        RADalign::download_RAD_data("MetaScope", selected_taxa())
      }
    } else if (download_pipeline() == "kraken") {

    } else if (download_pipeline() == "qiime2") {

    }
  })

  # choose which page to show
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

  # rebuild the plot when relevant inputs change
  msa_plot <- shiny::reactive({
    selected_vregions()
    input$continueWithTaxa
    input$detailedView
    input$vregionIDs

    make_msa_plotly(
      RADq = RADq(),
      unique = uniqueRADq(),
      groups = RADqGroups(),
      varRegions = selected_vregions(),
      detailed = input$detailedView,
      vregionIDs = input$vregionIDs
    )
  })

  # render the plotly object
  output$visual <- plotly::renderPlotly({
    msa_plot()
  })

  ###################################################################### CONTROLS FOR METASCOPE EXPORT SCREENS

  # store selected taxa from metascope filter picker
  shiny::observeEvent(input$selectTaxaFilter, {
    selected_taxa_metascope_filter(input$selectTaxaFilter %||% character(0))
  }, ignoreInit = TRUE)

  # enable or disable metascope filter card
  shiny::observe({
    set_metascope_filter_card_state(isTRUE(input$useMetascopeFilters))
  })

  # update whole-genus metascope filter label
  shiny::observeEvent(input$selectGenusFilter, {
    selected_genera_filter <- input$selectGenusFilter %||% character(0)
    n_genera <- length(selected_genera_filter)

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

  # fill metascope species filter with all members of selected genera
  shiny::observeEvent(input$entireGenusFilter, {
    shiny::req(input$selectGenusFilter)

    if (isTRUE(input$entireGenusFilter)) {
      shiny::updateSelectizeInput(
        session,
        "selectTaxaFilter",
        selected = get_species_from_genera(input$selectGenusFilter),
        server = TRUE
      )
    }
  }, ignoreInit = TRUE)



  # helper to enable or mute metascope filter controls
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


  # helper to update whole-genus checkbox labels
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
}
