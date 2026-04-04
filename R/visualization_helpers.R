# helper functions for RADexplorer plotting

#' Standardize plot inputs for RADexplorer visualizations
#'
#' @param RADq Data frame of RADq results.
#' @param unique Data frame of summarized unique IDs.
#' @param groups Data frame of taxa grouping information.
#' @param varRegions List of the user-selected variable regions
#'
#' @return a list containing the standardized unique, RADqtiles, groups_info, and copy_counts
#'
#' @export
#'
standardize_plot_inputs <- function(RADq, unique, groups, varRegions) {

  # standardize column names in unique and groups
  if ("species" %in% names(unique) && !"taxa" %in% names(unique)) {
    unique <- unique |> dplyr::rename(taxa = species)
  }

  if ("species" %in% names(groups) && !"taxa" %in% names(groups)) {
    groups <- groups |> dplyr::rename(taxa = species)
  }

  if ("groups" %in% names(groups) && !"group" %in% names(groups)) {
    groups <- groups |> dplyr::rename(group = groups)
  }

  # create RADqtiles, keep only the RADq columns used for plotting
  RADqtiles <- RADq |>
    dplyr::distinct(species, copy_id, variable_region, seq_id) |>
    dplyr::group_by(species) |>
    dplyr::mutate(
      copy_num = dplyr::dense_rank(copy_id),
      variable_region_clean = variable_region
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      species,
      variable_region_clean,
      copy_num,
      seq_id
    ) |>
    dplyr::filter(!is.na(copy_num), !is.na(seq_id)) |>
    dplyr::distinct(species, variable_region_clean, copy_num, .keep_all = TRUE)

  # creates groups_info, a copy of the groups df with the species with a group column
  groups_info <- groups |>
    dplyr::select(taxa, group) |>
    dplyr::mutate(
      taxa_order = dplyr::row_number(),
      group_num = match(group, unique(group)),
      group_label = paste0("Group ", group_num)
    )

  # copy counts per taxa, derived from RADqtiles
  copy_counts <- RADqtiles |>
    dplyr::distinct(species, copy_num) |>
    dplyr::count(species, name = "n_copies") |>
    dplyr::rename(taxa = species)

  list(
    unique = unique,
    RADqtiles = RADqtiles,
    groups_info = groups_info,
    copy_counts = copy_counts
  )
}

#' Build the species_layout dataframe for the visualization
#'
#' @param RADqtiles ...
#' @param groups_info ...
#' @param gap A changeable int that determines the vertical gap between species in a column
#'
#' @return a list containing species_layout, copy_layout, RADqtiles, y_breaks, group_bracket_df, and unique_taxa_df dfs
#'
#' @export
#'
build_species_layout <- function(RADqtiles, groups_info, gap = 2) {

  # create species_levels, a list with the ordering of the species based on grouping
  species_levels <- c(
    groups_info |>
      dplyr::filter(taxa %in% RADqtiles$species) |>
      dplyr::arrange(group_num, taxa_order) |>
      dplyr::pull(taxa),
    sort(setdiff(unique(RADqtiles$species), groups_info$taxa))
  ) |>
    unique()

  # creates species_counts, a copy of the RADqtiles df with a column with the number of gene copies per species
  species_counts <- RADqtiles |>
    dplyr::distinct(species, copy_num) |>
    dplyr::count(species, name = "n_copies") |>
    dplyr::mutate(species = factor(species, levels = species_levels)) |>
    dplyr::arrange(species)

  # creates species_layout, a df that creates the coordinates of the tiles in each column
  # species have a start and an end
  species_layout <- species_counts |>
    dplyr::mutate(
      start = dplyr::lag(cumsum(n_copies + gap), default = 0) + 1,
      end = start + n_copies - 1,
      y_lab = (start + end) / 2
    ) |>
    dplyr::left_join(
      groups_info |>
        dplyr::select(species = taxa, group, group_num, group_label, taxa_order),
      by = "species"
    )

  # creates copy_layout, a copy of the RADqtiles df that places the species and gene copies in the order that they will be plotted
  copy_layout <- RADqtiles |>
    dplyr::distinct(species, copy_num) |>
    dplyr::mutate(species = factor(species, levels = species_levels)) |>
    dplyr::arrange(species, copy_num) |>
    dplyr::group_by(species) |>
    dplyr::mutate(copy_row = dplyr::row_number()) |>
    dplyr::ungroup()

  # creates RADqtiles_plot, a copy of the RADqtiles df that joins species_layout and copy_layout
  RADqtiles_plot <- RADqtiles |>
    dplyr::mutate(
      species = factor(species, levels = species_levels),
      seq_id_local = factor(
        as.integer(factor(seq_id, levels = unique(seq_id)))
      )
    ) |>
    dplyr::left_join(
      species_layout |> dplyr::select(species, start),
      by = "species"
    ) |>
    dplyr::left_join(copy_layout, by = c("species", "copy_num")) |>
    dplyr::mutate(
      y = start + copy_row - 1,
      hover_text = paste0(species, " copy #", copy_num, " ", variable_region_clean)
    )

  #helper dfs for creating the grouping brackets
  y_breaks <- species_layout |>
    dplyr::arrange(species) |>
    dplyr::select(species, y_lab, n_copies)

  group_sizes <- species_layout |>
    dplyr::count(group_label, name = "n_species")

  # creates group_bracket_df, a copy of the species_layout df that finds start and end y coordinates for the grouping brackets
  group_bracket_df <- species_layout |>
    dplyr::group_by(group_label) |>
    dplyr::summarise(
      y_start = min(start) - 0.38,
      y_end = max(end) + 0.38,
      .groups = "drop"
    ) |>
    dplyr::left_join(group_sizes, by = "group_label") |>
    dplyr::filter(n_species > 1) |>
    dplyr::select(-n_species)

  unique_taxa_df <- species_layout |>
    dplyr::left_join(group_sizes, by = "group_label") |>
    dplyr::filter(n_species == 1) |>
    dplyr::distinct(species, y_lab)

  list(
    species_layout = species_layout,
    copy_layout = copy_layout,
    RADqtiles = RADqtiles_plot,
    y_breaks = y_breaks,
    group_bracket_df = group_bracket_df,
    unique_taxa_df = unique_taxa_df
  )
}

#' Build species y-axis labels
#'
#' @param species List of user-selected genus species names
#' @param n_copies List of number of copies
#'
#' @return a list containing species labels for the y axis
#' @export
#'
make_species_axis_labels <- function(species, n_copies, searched_taxa = character(0)) {

  searched_taxa <- searched_taxa %||% character(0)

  arrow <- "<span style='font-size:18pt; color:#00dfeb; font-weight:650; position:relative; top:8px;'>\u27A4  </span>"

  species_label <- paste0(
    ifelse(species %in% searched_taxa, arrow, ""),
    "<span style='font-size:10pt; line-height:1.1; font-weight:650;'><i>",
    species,
    "</i></span>"
  )

  paste0(
    species_label,
    "<br><span style='font-size:8pt; line-height:1.1;'>",
    n_copies,
    " 16S rRNA gene cop",
    ifelse(n_copies == 1, "y", "ies"),
    "</span>"
  )
}

#' Build plotly layout
#'
#' @param p_msa a ggplot object of the visualization
#' @param plot_height Int - height of the plot
#'
#' @return A plotly object of the final visualization
#' @export
#'
make_plotly_layout <- function(p_msa, plot_height) {

  # final plotly layout
  plotly::ggplotly(p_msa, tooltip = c("text", "seq_id")) |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      margin = list(l = 120, r = 30, t = 40, b = 20),
      height = plot_height,
      annotations = list(
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
      ),
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

#' Build tile palette
#'
#' @param ids list of ids for visual tiles
#' @param seed int used as a seed for random assignment
#'
#' @return a vector of colors (in HCL), one color randomly assigned per tile
#' @export
#'
build_tile_palette <- function(ids, seed = NULL) {
  tile_levels <- sort(unique(as.character(ids)))

  tile_palette <- grDevices::hcl(
    h = seq(15, 375, length.out = length(tile_levels) + 1)[seq_along(tile_levels)],
    c = 100,
    l = 65
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  tile_palette <- sample(tile_palette)
  names(tile_palette) <- tile_levels
  tile_palette
}

#' Build selected v-region rectangles
#'
#' @param selected_vr list of selected v-regions
#' @param vr_levels_all list of all v-regions
#' @param ymin int for the top of the rectangles
#' @param ymax int for the bottom of the rectangles
#'
#' @return a tibble with x and y coordinates in order to draw each of the selected v-region rectangles
#' @export
#'
build_selected_vr_rects <- function(selected_vr, vr_levels_all, ymin, ymax) {
  tibble::tibble(vx = match(selected_vr, vr_levels_all)) |>
    dplyr::filter(!is.na(vx)) |>
    dplyr::distinct() |>
    dplyr::transmute(
      xmin = vx - 0.48,
      xmax = vx + 0.48,
      ymin = ymin,
      ymax = ymax
    )
}

#' Add selected v-region rectangles
#'
#' @param p ggplot object, the current plot
#' @param selected_vr_rects a tibble with x and y coordinates in order to draw each of the selected v-region rectangles
#'
#' @return p, a ggplot object, with the added golden rectangles drawn at the coordinates provided in selected_vr_rects
#' @export
#'
add_selected_vr_rects <- function(p, selected_vr_rects) {
  if (nrow(selected_vr_rects) == 0) {
    return(p)
  }

  p +
    ggplot2::geom_rect(
      data = selected_vr_rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "gold",
      alpha = 0.75,
      color = NA
    )
}

#' Add group brackets
#'
#' @param p ggplot object, the current plot
#' @param group_bracket_df a data frame with y coordinates for each of the grouping brackets
#' @param bracket_x
#'
#' @return p, a ggplot object, with the added red brackets for each of the grouped taxa
#' @export
#'
add_group_brackets <- function(p, group_bracket_df, bracket_x, bracket_arm) {
  if (nrow(group_bracket_df) == 0) {
    return(p)
  }

  p +
    ggplot2::geom_segment(
      data = group_bracket_df,
      ggplot2::aes(y = y_start, yend = y_end),
      x = bracket_x, xend = bracket_x,
      inherit.aes = FALSE,
      color = "red",
      linewidth = 0.75,
      lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = group_bracket_df,
      ggplot2::aes(y = y_start, yend = y_start),
      x = bracket_x, xend = bracket_x + bracket_arm,
      inherit.aes = FALSE,
      color = "red",
      linewidth = 0.75,
      lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = group_bracket_df,
      ggplot2::aes(y = y_end, yend = y_end),
      x = bracket_x, xend = bracket_x + bracket_arm,
      inherit.aes = FALSE,
      color = "red",
      linewidth = 0.75,
      lineend = "round"
    )
}

#' Add unique taxa checkmarks
#'
#' @param p ggplot object, the current plot
#' @param unique_taxa_df a data frame with all unique taxa
#' @param x int -- the x coordinate of the checkmarks on the plot
#' @param y_col lsit of y coordinates for each of the checkmarks
#'
#' @return p, a ggplot object, with the added green checkmarks for each of the grouped taxa
#' @export
#'
add_unique_taxa_checks <- function(p, unique_taxa_df, x, y_col) {
  if (nrow(unique_taxa_df) == 0) {
    return(p)
  }

  p +
    ggplot2::geom_text(
      data = unique_taxa_df,
      ggplot2::aes(x = x, y = .data[[y_col]]),
      label = "\u2714",
      inherit.aes = FALSE,
      color = "green3",
      size = 4
    )
}

#' Add tile ids
#'
#' @param p ggplot object, the current plot
#' @param data a data frame with the tiles and their ids
#' @param x_col -- x coordinates for each of the 9 v-region columns
#' @param y_col lsit of y coordinates for each of the tile rows
#' @param enabled boolean for whether tile ids are enabled by the user
#' @param size int -- font size
#'
#' @return p, a ggplot object, with the added green checkmarks for each of the grouped taxa
#' @export
#'
add_tile_ids <- function(p, data, label_col, x_col, y_col, enabled, size) {
  if (!isTRUE(enabled)) {
    return(p)
  }

  p +
    ggplot2::geom_text(
      data = data,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data[[y_col]] + 0.08,
        label = as.character(.data[[label_col]])
      ),
      inherit.aes = FALSE,
      color = "white",
      size = size,
      fontface = "bold"
    )
}
