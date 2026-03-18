# helper for non-detailed RADexplorer plot

library(tidyverse)
library(ggtext)

build_nondetailed_plot <- function(unique, groups_info, RADq, selected_regions_clean, selected_vr, vr_levels_all, vregionIDs = FALSE) {

  gap <- 1.5

  groups_plot <- unique %>%
    dplyr::select(taxa, dplyr::any_of(selected_vr)) %>%
    tidyr::pivot_longer(
      cols = -taxa,
      names_to = "vregion",
      values_to = "group"
    ) %>%
    dplyr::mutate(
      vregion = factor(vregion, levels = vr_levels_all),
      taxa = as.character(taxa),
      group = as.character(group)
    ) %>%
    dplyr::group_by(vregion) %>%
    dplyr::mutate(group_id = match(group, unique(group))) %>%
    dplyr::ungroup()

  taxa_levels <- c(
    groups_info %>%
      dplyr::filter(taxa %in% groups_plot$taxa) %>%
      dplyr::arrange(group_num, taxa_order) %>%
      dplyr::pull(taxa),
    groups_plot %>%
      dplyr::distinct(taxa) %>%
      dplyr::pull(taxa) %>%
      setdiff(groups_info$taxa) %>%
      sort()
  ) %>% unique()

  y_map <- tibble(
    taxa = taxa_levels,
    y = seq_along(taxa_levels) + (seq_along(taxa_levels) - 1) * gap
  )

  groups_plot <- groups_plot %>%
    dplyr::left_join(y_map, by = "taxa")

  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  bracket_x <- 0.28
  bracket_arm <- 0.10

  header_idx <- seq(1, nrow(y_map), by = 5)

  header_rows <- y_map[header_idx, , drop = FALSE] %>%
    dplyr::transmute(y_header = y - 1.4) %>%
    tidyr::crossing(
      tibble(
        vregion = vr_levels_all,
        vx = seq_along(vr_levels_all)
      )
    )

  copy_counts_nondetailed <- RADq %>%
    dplyr::transmute(
      taxa = species,
      variable_region = variable_region,
      copy_num = as.numeric(copy_num)
    ) %>%
    dplyr::filter(
      variable_region %in% selected_regions_clean,
      !is.na(copy_num)
    ) %>%
    dplyr::distinct(taxa, copy_num) %>%
    dplyr::count(taxa, name = "n_copies")

  y_map_labeled <- y_map %>%
    dplyr::left_join(copy_counts_nondetailed, by = "taxa") %>%
    dplyr::mutate(n_copies = ifelse(is.na(n_copies), 0, n_copies))

  group_bracket_df_nondetailed <- groups_plot %>%
    dplyr::distinct(taxa, y) %>%
    dplyr::left_join(
      groups_info %>% dplyr::select(taxa, group_label),
      by = "taxa"
    ) %>%
    dplyr::group_by(group_label) %>%
    dplyr::summarise(
      y_start = min(y) - 0.72,
      y_end = max(y) + 0.72,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      x = bracket_x,
      x_inner = bracket_x + bracket_arm
    )

  p_msa <- ggplot() +
    geom_text(
      data = header_rows,
      aes(x = vx, y = y_header + 0.25, label = vregion),
      inherit.aes = FALSE,
      color = "black",
      size = 2.8
    ) +
    geom_segment(
      data = group_bracket_df_nondetailed,
      aes(x = x, xend = x, y = y_start, yend = y_end),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.75,
      lineend = "round"
    ) +
    geom_segment(
      data = group_bracket_df_nondetailed,
      aes(x = x, xend = x_inner, y = y_start, yend = y_start),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.75,
      lineend = "round"
    ) +
    geom_segment(
      data = group_bracket_df_nondetailed,
      aes(x = x, xend = x_inner, y = y_end, yend = y_end),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.75,
      lineend = "round"
    ) +
    geom_tile(
      data = groups_plot %>% dplyr::filter(vregion %in% selected_vr),
      aes(x = match(vregion, vr_levels_all), y = y, fill = factor(group_id)),
      width = tile_w,
      height = 1.5,
      color = "black",
      linewidth = 0.35
    )

  if (isTRUE(vregionIDs)) {
    p_msa <- p_msa +
      geom_text(
        data = groups_plot %>% dplyr::filter(vregion %in% selected_vr),
        aes(x = match(vregion, vr_levels_all), y = y + 0.08, label = group_id),
        inherit.aes = FALSE,
        color = "white",
        size = 2.8,
        fontface = "bold"
      )
  }

  p_msa <- p_msa +
    scale_x_continuous(
      breaks = seq_len(n_vr),
      labels = NULL,
      position = "top",
      limits = c(0.05, n_vr + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = y_map_labeled$y,
      labels = make_species_axis_labels(y_map_labeled$taxa, y_map_labeled$n_copies),
      limits = c(max(groups_plot$y) + 0.8, min(header_rows$y_header) - 0.2),
      expand = c(0, 0),
      trans = "reverse"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(r = 18)),
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank()
    ) +
    labs(x = NULL, y = NULL)

  plot_height <- max(200, 80 + (max(y_map$y) * 25))

  list(
    plot = p_msa,
    plot_height = plot_height
  )
}
