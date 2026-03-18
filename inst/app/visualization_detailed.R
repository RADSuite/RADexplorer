# helper for detailed RADexplorer plot

library(tidyverse)
library(ggtext)

build_detailed_plot <- function(layout_data, vr_levels_all, vregionIDs = FALSE) {

  # unpack plotting inputs
  RADqtiles <- layout_data$RADqtiles %>%
    mutate(vx = match(variable_region_clean, vr_levels_all))

  copies_tbl <- layout_data$copies_tbl
  copy_map <- layout_data$copy_map
  y_breaks <- layout_data$y_breaks
  group_bracket_df <- layout_data$group_bracket_df
  unique_taxa_df <- layout_data$unique_taxa_df

  # set plotting constants
  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  backbone_pad <- 1.25
  backbone_width <- n_vr + 2 * backbone_pad - 1.5
  bracket_x <- -0.35
  bracket_arm <- 0.12
  check_x <- bracket_x + 0.03

  # build tile colors
  tile_palette <- make_detailed_tile_palette(RADqtiles)

  # make one backbone row per copy
  detailed_backbone_df <- copy_map %>%
    left_join(select(copies_tbl, species, start), by = "species") %>%
    mutate(y = start + copy_row - 1) %>%
    distinct(species, copy_num, copy_row, y)

  # repeat V-region labels above each species block
  species_header_df <- tibble(
    y_header = copies_tbl$start - 0.8
  ) %>%
    tidyr::crossing(
      tibble(
        variable_region_clean = vr_levels_all,
        vx = seq_along(vr_levels_all)
      )
    )

  # start the detailed plot
  p_msa <- ggplot() +
    geom_tile(
      data = detailed_backbone_df,
      aes(x = (n_vr + 1) / 2, y = y),
      inherit.aes = FALSE,
      fill = "grey80",
      color = NA,
      width = backbone_width,
      height = 0.20
    ) +
    geom_text(
      data = species_header_df,
      aes(x = vx, y = y_header, label = variable_region_clean),
      inherit.aes = FALSE,
      color = "black",
      size = 2.8
    ) +
    geom_tile(
      data = RADqtiles,
      aes(x = vx, y = y, fill = seq_id_local, text = hover_text),
      color = "black",
      width = tile_w,
      height = 0.75,
      linewidth = 0.35
    ) +
    scale_fill_manual(values = tile_palette)

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
        aes(x = check_x, y = y_lab),
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
        data = RADqtiles,
        aes(x = vx, y = y + 0.08, label = as.character(seq_id_local)),
        inherit.aes = FALSE,
        color = "white",
        size = 2.5,
        fontface = "bold"
      )
  }

  # finish axes and styling
  p_msa <- p_msa +
    scale_x_continuous(
      breaks = seq_len(n_vr),
      labels = NULL,
      position = "top",
      limits = c(0.3 - backbone_pad, n_vr + 1.2 + backbone_pad),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = y_breaks$y_lab,
      labels = make_species_axis_labels(y_breaks$species, y_breaks$n_copies),
      limits = c(max(detailed_backbone_df$y) + 0.5, min(species_header_df$y_header) - 0.6),
      expand = c(0, 0),
      trans = "reverse"
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = ggtext::element_markdown(margin = ggplot2::margin(r = 18)),
      strip.text = element_text(size = 12)
    )

  # scale plot height with number of rows
  plot_height <- max(500, 80 + max(detailed_backbone_df$y, 1, na.rm = TRUE) * 18)

  list(
    plot = p_msa,
    plot_height = plot_height
  )
}
