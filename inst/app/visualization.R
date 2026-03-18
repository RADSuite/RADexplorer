# main RADexplorer plotting function

sys.source(file.path(base_dir, "visualization_helpers.R"), envir = environment())
sys.source(file.path(base_dir, "visualization_detailed.R"), envir = environment())
sys.source(file.path(base_dir, "visualization_nondetailed.R"), envir = environment())

make_msa_plotly <- function(
    RADq,
    unique,
    groups,
    varRegions = c("V1","V2","V3","V4","V5","V6","V7","V8","V9"),
    highlight_unique = FALSE,
    detailed = TRUE,
    vregionIDs = FALSE,
    package = "RADexplorer"
) {

  # load packages
  library(tidyverse)
  library(plotly)
  library(ggtext)

  # user selected variable regions
  selected_regions_clean <- varRegions
  vr_levels_all <- paste0("V", 1:9)
  selected_vr <- selected_regions_clean

  # standardize and prep shared inputs
  prep <- standardize_plot_inputs(
    RADq = RADq,
    unique = unique,
    groups = groups,
    selected_regions_clean = selected_regions_clean
  )

  # build shared species layout for detailed mode
  layout_data <- build_species_layout(
    RADqtiles = prep$RADqtiles,
    groups_info = prep$groups_info,
    gap = 2
  )

  # build the requested plot mode
  if (isTRUE(detailed)) {
    built <- build_detailed_plot(
      layout_data = layout_data,
      vr_levels_all = vr_levels_all,
      vregionIDs = vregionIDs
    )
  } else {
    built <- build_nondetailed_plot(
      unique = prep$unique,
      groups_info = prep$groups_info,
      RADq = prep$RADq,
      selected_regions_clean = selected_regions_clean,
      selected_vr = selected_vr,
      vr_levels_all = vr_levels_all,
      vregionIDs = vregionIDs
    )
  }

  # convert ggplot to plotly and return
  make_plotly_layout(
    p_msa = built$plot,
    plot_height = built$plot_height
  )
}
