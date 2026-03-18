# helper functions for RADexplorer plotting

library(tidyverse)
library(ggtext)

standardize_plot_inputs <- function(RADq, unique, groups, selected_regions_clean) {

  # standardize column names
  if ("species" %in% names(unique) && !"taxa" %in% names(unique)) {
    unique <- unique %>% dplyr::rename(taxa = species)
  }

  if ("species" %in% names(groups) && !"taxa" %in% names(groups)) {
    groups <- groups %>% dplyr::rename(taxa = species)
  }

  if ("groups" %in% names(groups) && !"group" %in% names(groups)) {
    groups <- groups %>% dplyr::rename(group = groups)
  }

  # keep only the RADq columns used for plotting
  RADqtiles <- RADq %>%
    dplyr::filter(variable_region %in% selected_regions_clean) %>%
    dplyr::transmute(
      species,
      variable_region_clean = variable_region,
      copy_num = as.numeric(copy_num),
      seq_id
    ) %>%
    dplyr::filter(!is.na(copy_num), !is.na(seq_id)) %>%
    dplyr::distinct(species, variable_region_clean, copy_num, .keep_all = TRUE)

  # preserve input group order
  groups_info <- groups %>%
    dplyr::select(taxa, group) %>%
    dplyr::mutate(
      taxa_order = dplyr::row_number(),
      group_num = match(group, unique(group)),
      group_label = paste0("Group ", group_num)
    )

  list(
    RADq = RADq,
    unique = unique,
    groups = groups,
    RADqtiles = RADqtiles,
    groups_info = groups_info
  )
}

build_species_layout <- function(RADqtiles, groups_info, gap = 2) {

  # order taxa by grouping first
  species_levels <- c(
    groups_info %>%
      dplyr::filter(taxa %in% RADqtiles$species) %>%
      dplyr::arrange(group_num, taxa_order) %>%
      dplyr::pull(taxa),
    sort(setdiff(unique(RADqtiles$species), groups_info$taxa))
  ) %>% unique()

  # compute the y blocks for each species
  copies_tbl <- RADqtiles %>%
    dplyr::distinct(species, copy_num) %>%
    dplyr::count(species, name = "n_copies") %>%
    dplyr::mutate(species = factor(species, levels = species_levels)) %>%
    dplyr::arrange(species) %>%
    dplyr::mutate(
      start = dplyr::lag(cumsum(n_copies + gap), default = 0) + 1,
      end = start + n_copies - 1,
      y_lab = (start + end) / 2
    ) %>%
    dplyr::left_join(
      groups_info %>% dplyr::select(species = taxa, group, group_num, group_label, taxa_order),
      by = "species"
    )

  # map actual copy numbers to plotting rows
  copy_map <- RADqtiles %>%
    dplyr::distinct(species, copy_num) %>%
    dplyr::mutate(species = factor(species, levels = species_levels)) %>%
    dplyr::arrange(species, copy_num) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(copy_row = dplyr::row_number()) %>%
    dplyr::ungroup()

  # attach y coordinates to each tile
  RADqtiles2 <- RADqtiles %>%
    dplyr::mutate(
      species = factor(species, levels = species_levels),
      seq_id_local = factor(substring(seq_id, 3))
    ) %>%
    dplyr::left_join(copies_tbl %>% dplyr::select(species, start), by = "species") %>%
    dplyr::left_join(copy_map, by = c("species", "copy_num")) %>%
    dplyr::mutate(
      y = start + copy_row - 1,
      hover_text = paste0(species, " copy number ", copy_num)
    )

  # one axis label per species
  y_breaks <- copies_tbl %>%
    dplyr::arrange(species) %>%
    dplyr::select(species, y_lab, n_copies)

  # count taxa per group
  group_sizes <- copies_tbl %>%
    dplyr::count(group_label, name = "n_species")

  # bracket ranges for grouped taxa
  group_bracket_df <- copies_tbl %>%
    dplyr::group_by(group_label) %>%
    dplyr::summarise(
      y_start = min(start) - 0.38,
      y_end = max(end) + 0.38,
      .groups = "drop"
    ) %>%
    dplyr::left_join(group_sizes, by = "group_label") %>%
    dplyr::filter(n_species > 1) %>%
    dplyr::select(-n_species)

  # unique taxa get checkmarks
  unique_taxa_df <- copies_tbl %>%
    dplyr::left_join(group_sizes, by = "group_label") %>%
    dplyr::filter(n_species == 1) %>%
    dplyr::distinct(species, y_lab)

  list(
    species_levels = species_levels,
    copies_tbl = copies_tbl,
    copy_map = copy_map,
    RADqtiles = RADqtiles2,
    y_breaks = y_breaks,
    group_bracket_df = group_bracket_df,
    unique_taxa_df = unique_taxa_df
  )
}

make_detailed_tile_palette <- function(RADqtiles) {

  # colors for detailed seq IDs
  tile_levels <- sort(unique(substring(RADqtiles$seq_id, 3)))
  tile_palette <- grDevices::hcl.colors(
    max(length(tile_levels), 3),
    palette = "Set 2"
  )[seq_along(tile_levels)]
  names(tile_palette) <- tile_levels
  tile_palette
}

make_nondetailed_tile_palette <- function(groups_plot) {

  # colors for non-detailed group IDs
  tile_group_levels <- sort(unique(groups_plot$group_id))
  tile_group_palette <- grDevices::hcl.colors(
    max(length(tile_group_levels), 3),
    palette = "Set 2"
  )[seq_along(tile_group_levels)]
  names(tile_group_palette) <- as.character(tile_group_levels)
  tile_group_palette
}

make_species_axis_labels <- function(species, n_copies) {

  # species name plus copy count label
  paste0(
    "<span style='font-size:10pt; line-height:1.1; font-weight:650;'><i>", species, "</i></span>",
    "<br><span style='font-size:8pt; line-height:1.1;'>",
    n_copies,
    " 16S rRNA gene cop",
    ifelse(n_copies == 1, "y", "ies"),
    "</span>"
  )
}

make_left_titles <- function() {

  # left-side plotly titles
  list(
    list(
      x = -0.1,
      y = 1,
      yshift = 18,
      xref = "paper",
      yref = "paper",
      text = "<b>Taxa</b>",
      showarrow = FALSE,
      xanchor = "center",
      yanchor = "bottom",
      font = list(size = 15, color = "black")
    ),
    list(
      x = 0.5,
      y = 1,
      yshift = 18,
      xref = "paper",
      yref = "paper",
      text = "<b>16S rRNA Gene Copies</b>",
      showarrow = FALSE,
      xanchor = "center",
      yanchor = "bottom",
      font = list(size = 15, color = "black")
    )
  )
}

make_plotly_layout <- function(p_msa, plot_height) {

  # final plotly layout
  ggplotly(p_msa, tooltip = c("text", "seq_id")) %>%
    layout(
      margin = list(l = 220, r = 30, t = 140, b = 40),
      height = plot_height,
      annotations = make_left_titles(),
      xaxis = list(
        side = "top",
        showline = FALSE,
        zeroline = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        showline = FALSE,
        zeroline = FALSE,
        showgrid = FALSE
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
}
