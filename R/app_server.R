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

  # app state
  screen <- shiny::reactiveVal("menu")
  selected_taxa <- shiny::reactiveVal(character(0))
  selected_vregions <- shiny::reactiveVal(c("V4"))
  RADq <- shiny::reactiveVal(NULL)
  uniqueRADq <- shiny::reactiveVal(NULL)
  RADqGroups <- shiny::reactiveVal(NULL)
  loading <- shiny::reactiveVal(FALSE)
  radx_search_seed_taxa <- shiny::reactiveVal(character(0))
  radx_search_display_taxa <- shiny::reactiveVal(character(0))
  # genus-wide dropdown pattern
  genus_all_species_pattern <- " - All Species(?: \\([0-9]+\\))?$"
  # get accessions table from RADalign
  accessions_table <- RADalign::get_accessions_df()
  # helper to detect genus-wide dropdown options
  is_genus_all_species <- function(x) {
    grepl(genus_all_species_pattern, x)
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
  # helper for RADx search
  radx_search_taxa <- function(search_taxa, groups_df, all_taxa) {
    search_taxa <- search_taxa %||% character(0)
    all_taxa <- all_taxa %||% character(0)
    if (length(all_taxa) == 0) {
      return(character(0))
    }
    if (length(search_taxa) == 0) {
      return(sort(unique(all_taxa)))
    }
    if (is.null(groups_df) || nrow(groups_df) == 0) {
      return(sort(unique(search_taxa)))
    }
    matched_groups <- unique(groups_df$groups[groups_df$taxa %in% search_taxa])
    matched_groups <- matched_groups[!is.na(matched_groups)]
    grouped_taxa <- groups_df$taxa[groups_df$groups %in% matched_groups]
    sort(unique(c(search_taxa, grouped_taxa)))
  }
  # rebuild taxa picker when entering menu
  shiny::observeEvent(screen(), {
    shiny::req(screen() == "menu")

    organisms <- load_organism_selection_list(accessions_table)

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "selectTaxa",
      choices = organisms,
      choicesOpt = list(
        style = ifelse(
          is_genus_all_species(organisms),
          "font-weight:700; font-style:italic;",
          "font-style:italic;"
        )
      ),
      selected = selected_taxa()
    )
  }, ignoreInit = FALSE)
  # rebuild RADx search picker when entering RADx
  shiny::observeEvent(screen(), {
    shiny::req(screen() == "radx")
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "radxSearchTaxa",
      choices = sort(unique(expand_selected_taxa(selected_taxa()))),
      selected = character(0)
    )
  }, ignoreInit = TRUE)
  # note under the taxa selector
  output$speciesNote <- shiny::renderUI({
    n_selected <- length(expand_selected_taxa(selected_taxa()))
    tags$div(
      style = "margin:0; padding:0;",
      tags$i(
        paste0(
          "You have selected ", n_selected, " taxa."
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
      radx_search_seed_taxa(c())
      radx_search_display_taxa(taxa_now)
      loading(FALSE)
      screen("radx")
    }, delay = 0)
  })
  # apply RADx search
  shiny::observeEvent(input$radxSearchTaxa, {
    shiny::req(RADqGroups())
    all_taxa_now <- sort(unique(expand_selected_taxa(selected_taxa())))
    seed_taxa_now <- input$radxSearchTaxa
    radx_search_seed_taxa(seed_taxa_now)
    radx_search_display_taxa(
      radx_search_taxa(
        search_taxa = seed_taxa_now,
        groups_df = RADqGroups(),
        all_taxa = all_taxa_now
      )
    )
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  # update selected variable regions
  shiny::observeEvent(input$varRegions, {
    shiny::req(RADq(), uniqueRADq())
    vr <- input$varRegions %||% character(0)
    selected_vregions(vr)
    new_groups <- tryCatch(
      RADalign::createRADqGroups(if (length(vr) == 0) paste0("V", 1:9) else vr, TRUE),
      error = function(e) NULL
    )
    if (!is.null(new_groups) && nrow(new_groups) > 0) {
      RADqGroups(new_groups)
      radx_search_display_taxa(
        radx_search_taxa(
          search_taxa = radx_search_seed_taxa(),
          groups_df = new_groups,
          all_taxa = sort(unique(expand_selected_taxa(selected_taxa())))
        )
      )
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
    radx_search_seed_taxa(character(0))
    radx_search_display_taxa(character(0))
    screen("menu")
  })
  shiny::observeEvent(input$download, {
    screen("RADport")
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
    }
  })
  # rebuild the plot when inputs change
  msa_plot <- shiny::reactive({
    selected_vregions()
    input$continueWithTaxa
    input$detailedView
    input$vregionIDs
    input$radxSearchTaxa
    display_taxa <- radx_search_display_taxa()
    if (length(display_taxa) == 0) {
      display_taxa <- sort(unique(expand_selected_taxa(selected_taxa())))
    }
    filtered_RADq <- RADq()
    if (!is.null(filtered_RADq)) {
      filtered_RADq <- filtered_RADq[filtered_RADq$species %in% display_taxa, , drop = FALSE]
    }
    filtered_unique <- uniqueRADq()
    if (!is.null(filtered_unique)) {
      filtered_unique <- filtered_unique[filtered_unique$species %in% display_taxa, , drop = FALSE]
    }
    filtered_groups <- RADqGroups()
    if (!is.null(filtered_groups)) {
      filtered_groups <- filtered_groups[filtered_groups$taxa %in% display_taxa, , drop = FALSE]
    }
    make_msa_plotly(
      RADq = filtered_RADq,
      unique = filtered_unique,
      groups = filtered_groups,
      varRegions = selected_vregions(),
      detailed = input$detailedView,
      vregionIDs = input$vregionIDs,
      searched_taxa = radx_search_seed_taxa()
    )
  })
  # render the plotly object
  output$visual <- plotly::renderPlotly({
    msa_plot()
  })
}
