# helper for non-detailed RADexplorer plot

build_nondetailed_plot <- function(unique, groups_info, RADq, selected_regions_clean, selected_vr, vr_levels_all, vregionIDs = FALSE) {

  # plotting inputs
  groups_plot <- unique |>
    dplyr::select(taxa, dplyr::any_of(vr_levels_all)) |>
    tidyr::pivot_longer(-taxa, names_to = "vregion", values_to = "group") |>
    dplyr::mutate(
      taxa = as.character(taxa),
      vregion = factor(vregion, levels = vr_levels_all),
      group = as.character(group),
      vx = match(vregion, vr_levels_all)
    ) |>
    dplyr::group_by(vregion) |>
    dplyr::mutate(group_id = match(group, unique(group))) |>
    dplyr::ungroup()

  # plotting constants
  gap <- 1.5
  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  bracket_x <- 0.28
  bracket_arm <- 0.10
  check_x <- bracket_x + 0.03

  # y positions
  taxa_levels <- c(
    groups_info |>
      dplyr::filter(taxa %in% groups_plot$taxa) |>
      dplyr::arrange(group_num, taxa_order) |>
      dplyr::pull(taxa),
    sort(setdiff(unique(groups_plot$taxa), groups_info$taxa))
  ) |>
    unique()

  y_map <- tibble::tibble(
    taxa = taxa_levels,
    y = seq(1, by = 1 + gap, length.out = length(taxa_levels))
  )

  groups_plot <- dplyr::left_join(groups_plot, y_map, by = "taxa")

  # repeated header labels
  header_rows <- tidyr::crossing(
    tibble::tibble(y_header = y_map$y - 1.4),
    tibble::tibble(vregion = vr_levels_all, vx = seq_along(vr_levels_all))
  )

  # highlighted variable-region columns
  selected_vr_rects <- build_selected_vr_rects(
    selected_vr = selected_vr,
    vr_levels_all = vr_levels_all,
    ymin = min(header_rows$y_header) - 0.1,
    ymax = max(y_map$y) + 0.8
  )

  # axis label data
  y_breaks <- y_map |>
    dplyr::left_join(
      count_species_copies(RADq),
      by = "taxa"
    ) |>
    dplyr::mutate(n_copies = tidyr::replace_na(n_copies, 0))

  # grouped taxon brackets
  group_summary <- y_map |>
    dplyr::left_join(
      dplyr::select(groups_info, taxa, group, group_label),
      by = "taxa"
    ) |>
    dplyr::add_count(group, name = "n_taxa")

  group_bracket_df <- group_summary |>
    dplyr::filter(n_taxa > 1) |>
    dplyr::group_by(group, group_label) |>
    dplyr::summarise(
      y_start = min(y) - 0.72,
      y_end = max(y) + 0.72,
      .groups = "drop"
    )

  # unique taxon checks
  unique_taxa_df <- group_summary |>
    dplyr::filter(n_taxa == 1) |>
    dplyr::distinct(taxa, y)

  # base plot
  p_msa <- ggplot2::ggplot()

  p_msa <- add_selected_vr_rects(p_msa, selected_vr_rects)

  p_msa <- p_msa +
    ggplot2::geom_text(
      data = header_rows,
      ggplot2::aes(x = vx, y = y_header + 0.6, label = vregion),
      inherit.aes = FALSE,
      color = "black",
      size = 2.8
    ) +
    ggplot2::geom_segment(
      data = y_map,
      ggplot2::aes(x = 0.6, xend = n_vr + 0.4, y = y, yend = y),
      inherit.aes = FALSE,
      color = "grey80",
      linewidth = 1.2,
      lineend = "round"
    ) +
    ggplot2::geom_tile(
      data = groups_plot,
      ggplot2::aes(x = vx, y = y, fill = factor(group_id)),
      width = tile_w,
      height = 1,
      color = "black",
      linewidth = 0.35
    )

  # grouped taxon brackets
  p_msa <- add_group_brackets(
    p = p_msa,
    group_bracket_df = group_bracket_df,
    bracket_x = bracket_x,
    bracket_arm = bracket_arm
  )

  # unique taxon checks
  p_msa <- add_unique_taxa_checks(
    p = p_msa,
    unique_taxa_df = unique_taxa_df,
    x = check_x,
    y_col = "y"
  )

  # tile IDs
  p_msa <- add_tile_ids(
    p = p_msa,
    data = groups_plot,
    label_col = "group_id",
    x_col = "vx",
    y_col = "y",
    enabled = vregionIDs,
    size = 2.8
  )

  # axes and theme
  p_msa <- p_msa +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n_vr),
      labels = NULL,
      position = "top",
      limits = c(0.05, n_vr + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks$y,
      labels = make_species_axis_labels(y_breaks$taxa, y_breaks$n_copies),
      limits = c(max(groups_plot$y) + 0.8, min(header_rows$y_header) - 0.2),
      expand = c(0, 0),
      trans = "reverse"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(r = 18)),
      axis.text.x.top = ggplot2::element_blank(),
      axis.ticks.x.top = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = NULL, y = NULL)

  # plot height
  n_taxa <- nrow(y_map)
  plot_height <- max(
    200,
    110 + 60 * n_taxa
  )

  # return plot and height
  list(plot = p_msa, plot_height = plot_height)
}
