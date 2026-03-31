#' Create the main RADexplorer MSA plot
#'
#' Builds the main interactive plot for RADexplorer from summarized
#' variable-region alignment data.
#'
#' @param RADq Data frame of RADq results.
#' @param unique Data frame of summarized unique IDs.
#' @param groups Data frame of taxa grouping information.
#' @param varRegions Character vector of variable regions to display.
#' @param highlight_unique Logical; whether to highlight unique regions.
#' @param detailed Logical; whether to build the detailed plot view.
#' @param vregionIDs Logical; whether to display variable-region IDs.
#' @param package Character scalar giving the package name.
#'
#' @return A plotly object.
#' @export
make_msa_plotly <- function(
    RADq,
    unique,
    groups,
    varRegions = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9"),
    highlight_unique = FALSE,
    detailed = TRUE,
    vregionIDs = FALSE,
    searched_taxa = character(0),
    package = "RADexplorer"
) {

  # user selected variable regions
  vr_levels_all <- paste0("V", 1:9)

  # standardize and prep shared inputs
  prep <- standardize_plot_inputs(
    RADq = RADq,
    unique = unique,
    groups = groups,
    varRegions = varRegions
  )

  # build the requested plot mode
  built <- if (isTRUE(detailed)) {
    layout_data <- build_species_layout(
      RADqtiles = prep$RADqtiles,
      groups_info = prep$groups_info,
      gap = 2
    )

    build_detailed_plot(
      layout_data = layout_data,
      vr_levels_all = vr_levels_all,
      unique = prep$unique,
      selected_vr = varRegions,
      vregionIDs = vregionIDs,
      searched_taxa = searched_taxa
    )
  } else {
    build_nondetailed_plot(
      unique = prep$unique,
      groups_info = prep$groups_info,
      RADq = prep$RADq,
      selected_regions_clean = varRegions,
      selected_vr = varRegions,
      vr_levels_all = vr_levels_all,
      vregionIDs = vregionIDs,
      searched_taxa = searched_taxa
    )
  }

  # convert ggplot to plotly and return
  make_plotly_layout(
    p_msa = built$plot,
    plot_height = built$plot_height
  )
}
