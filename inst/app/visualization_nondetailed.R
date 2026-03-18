# helper for non-detailed RADexplorer plot

library(tidyverse)
library(ggtext)

build_nondetailed_plot <- function(unique, groups_info, RADq, selected_regions_clean, selected_vr, vr_levels_all, vregionIDs = FALSE) {

  # set plot constants
  gap <- 1.5
  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  bracket_x <- 0.28
  bracket_arm <- 0.10
  check_x <- bracket_x + 0.03

  # reshape summarized IDs to long format
  groups_plot <- unique %>%
    select(taxa, any_of(selected_vr)) %>%
    pivot_longer(
      cols = -taxa,
      names_to = "vregion",
      values_to = "group"
    ) %>%
    mutate(
      vregion = factor(vregion, levels = vr_levels_all),
      taxa = as.character(taxa),
      group = as.character(group)
    ) %>%
    group_by(vregion) %>%
    mutate(group_id = match(group, unique(group))) %>%
    ungroup()

  # order taxa by group, then input order
  taxa_levels <- c(
    groups_info %>%
      filter(taxa %in% groups_plot$taxa) %>%
      arrange(group_num, taxa_order) %>%
      pull(taxa),
    groups_plot %>%
      distinct(taxa) %>%
      pull(taxa) %>%
      setdiff(groups_info$taxa) %>%
      sort()
  ) %>% unique()

  # assign y positions
  y_map <- tibble(
    taxa = taxa_levels,
    y = seq_along(taxa_levels) + (seq_along(taxa_levels) - 1) * gap
  )

  # add y positions to plotting data
  groups_plot <- groups_plot %>%
    left_join(y_map, by = "taxa")

  # repeat V-region labels above the plot
  header_rows <- tibble(
    y_header = y_map$y[seq(1, nrow(y_map), by = 5)] - 1.4
  ) %>%
    tidyr::crossing(
      tibble(
        vregion = vr_levels_all,
        vx = seq_along(vr_levels_all)
      )
    )

  # count gene copies for y-axis labels
  y_map_labeled <- y_map %>%
    left_join(
      RADq %>%
        transmute(
          taxa = species,
          variable_region = variable_region,
          copy_num = as.numeric(copy_num)
        ) %>%
        filter(variable_region %in% selected_regions_clean, !is.na(copy_num)) %>%
        distinct(taxa, copy_num) %>%
        count(taxa, name = "n_copies"),
      by = "taxa"
    ) %>%
    mutate(n_copies = ifelse(is.na(n_copies), 0, n_copies))

  # determine which taxa are grouped vs unique
  group_status_df <- y_map %>%
    left_join(
      groups_info %>% select(taxa, group, group_label),
      by = "taxa"
    )

  group_sizes <- group_status_df %>%
    count(group, name = "n_taxa")

  # make bracket ranges for grouped taxa
  group_bracket_df <- group_status_df %>%
    left_join(group_sizes, by = "group") %>%
    filter(n_taxa > 1) %>%
    group_by(group, group_label) %>%
    summarise(
      y_start = min(y) - 0.72,
      y_end = max(y) + 0.72,
      .groups = "drop"
    )

  # keep unique taxa for green checks
  unique_taxa_df <- group_status_df %>%
    left_join(group_sizes, by = "group") %>%
    filter(n_taxa == 1) %>%
    distinct(taxa, y)

  # start the non-detailed plot
  p_msa <- ggplot() +
    geom_text(
      data = header_rows,
      aes(x = vx, y = y_header + 0.25, label = vregion),
      inherit.aes = FALSE,
      color = "black",
      size = 2.8
    ) +
    geom_tile(
      data = groups_plot %>% filter(vregion %in% selected_vr),
      aes(x = match(vregion, vr_levels_all), y = y, fill = factor(group_id)),
      width = tile_w,
      height = 1.5,
      color = "black",
      linewidth = 0.35
    )

  # add red brackets for grouped taxa
  if (nrow(group_bracket_df) > 0) {
    p_msa <- p_msa +
      geom_segment(
        data = group_bracket_df,
        aes(y = y_start, yend = y_end),
        x = bracket_x,
        xend = bracket_x,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      ) +
      geom_segment(
        data = group_bracket_df,
        aes(y = y_start, yend = y_start),
        x = bracket_x,
        xend = bracket_x + bracket_arm,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      ) +
      geom_segment(
        data = group_bracket_df,
        aes(y = y_end, yend = y_end),
        x = bracket_x,
        xend = bracket_x + bracket_arm,
        inherit.aes = FALSE,
        color = "red3",
        linewidth = 0.75,
        lineend = "round"
      )
  }

  # add green checks for unique taxa
  if (nrow(unique_taxa_df) > 0) {
    p_msa <- p_msa +
      geom_text(
        data = unique_taxa_df,
        aes(x = check_x, y = y),
        label = "✔",
        inherit.aes = FALSE,
        color = "green3",
        size = 4
      )
  }

  # add tile labels if requested
  if (isTRUE(vregionIDs)) {
    p_msa <- p_msa +
      geom_text(
        data = groups_plot %>% filter(vregion %in% selected_vr),
        aes(x = match(vregion, vr_levels_all), y = y + 0.08, label = group_id),
        inherit.aes = FALSE,
        color = "white",
        size = 2.8,
        fontface = "bold"
      )
  }

  # finish axes and styling
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

  # scale plot height with number of taxa
  plot_height <- max(200, 80 + max(y_map$y) * 25)

  list(
    plot = p_msa,
    plot_height = plot_height
  )
}
