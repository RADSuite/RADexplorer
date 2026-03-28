# helper for detailed RADexplorer plot

build_detailed_plot <- function(layout_data, vr_levels_all, unique, selected_vr, vregionIDs = FALSE) {

  # plotting inputs
  species_layout <- layout_data$species_layout
  copy_layout <- layout_data$copy_layout
  y_breaks <- layout_data$y_breaks
  group_bracket_df <- layout_data$group_bracket_df
  unique_taxa_df <- layout_data$unique_taxa_df
  RADqtiles <- layout_data$RADqtiles |>
    dplyr::mutate(vx = match(variable_region_clean, vr_levels_all))

  # plotting constants
  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  backbone_pad <- 1.25
  backbone_width <- n_vr + 2 * backbone_pad - 1.5
  bracket_x <- -0.35
  bracket_arm <- 0.12
  check_x <- bracket_x + 0.03

  # tile colors
  tile_levels <- sort(unique(substring(RADqtiles$seq_id, 3)))
  tile_palette <- grDevices::hcl(
    h = seq(15, 375, length.out = length(tile_levels) + 1)[seq_along(tile_levels)],
    c = 100,
    l = 65
  )
  names(tile_palette) <- tile_levels

  # backbone rows
  detailed_backbone_df <- copy_layout |>
    dplyr::left_join(dplyr::select(species_layout, species, start), by = "species") |>
    dplyr::mutate(y = start + copy_row - 1) |>
    dplyr::distinct(species, copy_num, copy_row, y)

  # repeated header labels
  header_rows <- tidyr::crossing(
    tibble::tibble(species = species_layout$species, y_header = species_layout$start - 0.8),
    tibble::tibble(variable_region_clean = vr_levels_all, vx = seq_along(vr_levels_all))
  )

  # highlighted variable-region columns
  selected_vr_rects <- if (nrow(detailed_backbone_df) > 0) {
    tibble::tibble(vx = match(selected_vr, vr_levels_all)) |>
      dplyr::filter(!is.na(vx)) |>
      dplyr::distinct() |>
      dplyr::transmute(
        xmin = vx - 0.48,
        xmax = vx + 0.48,
        ymin = min(header_rows$y_header) - 0.4,
        ymax = max(detailed_backbone_df$y) + 0.5
      )
  } else {
    tibble::tibble(xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric())
  }

  unique_long <- unique |>
    dplyr::rename(species = taxa) |>
    tidyr::pivot_longer(2:10, names_to = "variable_region_clean", values_to = "unique") |>
    dplyr::mutate(unique = as.logical(unique))

  grouped_species <- setdiff(as.character(species_layout$species), as.character(unique_taxa_df$species))

  header_rows <- header_rows |>
    dplyr::left_join(unique_long, by = c("species", "variable_region_clean")) |>
    dplyr::mutate(show_star = !is.na(unique) & unique & species %in% grouped_species)

  # base plot
  p_msa <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = selected_vr_rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "gold",
      alpha = 0.75,
      color = NA
    ) +
    ggplot2::geom_tile(
      data = detailed_backbone_df,
      ggplot2::aes(x = (n_vr + 1) / 2, y = y),
      inherit.aes = FALSE,
      fill = "grey80",
      color = NA,
      width = backbone_width,
      height = 0.20
    ) +
    ggplot2::geom_text(
      data = header_rows,
      ggplot2::aes(x = vx, y = y_header, label = ifelse(show_star, paste0("*", variable_region_clean), variable_region_clean)),
      inherit.aes = FALSE,
      color = "black",
      size = 2.8
    ) +
    ggplot2::geom_tile(
      data = RADqtiles,
      ggplot2::aes(x = vx, y = y, fill = seq_id_local, text = hover_text),
      color = "black",
      width = tile_w,
      height = 0.75,
      linewidth = 0.35
    ) +
    ggplot2::scale_fill_manual(values = tile_palette)

  # grouped taxon brackets
  if (nrow(group_bracket_df) > 0) {
    p_msa <- p_msa +
      ggplot2::geom_segment(
        data = group_bracket_df,
        ggplot2::aes(y = y_start, yend = y_end),
        x = bracket_x, xend = bracket_x,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = group_bracket_df,
        ggplot2::aes(y = y_start, yend = y_start),
        x = bracket_x, xend = bracket_x + bracket_arm,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = group_bracket_df,
        ggplot2::aes(y = y_end, yend = y_end),
        x = bracket_x, xend = bracket_x + bracket_arm,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      )
  }

  # unique taxon checks
  if (nrow(unique_taxa_df) > 0) {
    p_msa <- p_msa +
      ggplot2::geom_text(
        data = unique_taxa_df,
        ggplot2::aes(x = check_x, y = y_lab),
        label = "✔",
        inherit.aes = FALSE,
        color = "green3",
        size = 4
      )
  }

  # tile IDs
  if (isTRUE(vregionIDs)) {
    p_msa <- p_msa +
      ggplot2::geom_text(
        data = RADqtiles,
        ggplot2::aes(x = vx, y = y + 0.08, label = as.character(seq_id_local)),
        inherit.aes = FALSE,
        color = "white",
        size = 2.5,
        fontface = "bold"
      )
  }

  # axes and theme
  p_msa <- p_msa +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n_vr),
      labels = NULL,
      position = "top",
      limits = c(0.3 - backbone_pad, n_vr + 1.2 + backbone_pad),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks$y_lab,
      labels = make_species_axis_labels(y_breaks$species, y_breaks$n_copies),
      limits = c(max(detailed_backbone_df$y) + 0.5, min(header_rows$y_header) - 0.6),
      expand = c(0, 0),
      trans = "reverse"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(r = 18)),
      strip.text = ggplot2::element_text(size = 12)
    )

  # plot height
  plot_height <- max(500, 80 + max(detailed_backbone_df$y, 1, na.rm = TRUE) * 18)

  # return plot and height
  list(plot = p_msa, plot_height = plot_height)
}
