# helper for detailed RADexplorer plot

library(tidyverse)
library(ggtext)

build_detailed_plot <- function(layout_data, vr_levels_all, vregionIDs = FALSE) {

  RADqtiles <- layout_data$RADqtiles
  copies_tbl <- layout_data$copies_tbl
  copy_map <- layout_data$copy_map
  y_breaks <- layout_data$y_breaks
  group_bracket_df <- layout_data$group_bracket_df

  n_vr <- length(vr_levels_all)
  tile_w <- 0.7
  backbone_pad <- 1.25
  bracket_x <- -0.35
  bracket_arm <- 0.12
  backbone_width <- n_vr + 2 * backbone_pad - 1.5

  tile_palette <- make_detailed_tile_palette(RADqtiles)

  RADqtiles <- RADqtiles %>%
    dplyr::mutate(vx = match(variable_region_clean, vr_levels_all))

  detailed_backbone_df <- copy_map %>%
    dplyr::left_join(copies_tbl %>% dplyr::select(species, start), by = "species") %>%
    dplyr::mutate(y = start + copy_row - 1) %>%
    dplyr::distinct(species, copy_num, copy_row, y)

  species_header_df <- copies_tbl %>%
    dplyr::transmute(y_header = start - 0.8) %>%
    tidyr::crossing(
      tibble(
        variable_region_clean = vr_levels_all,
        vx = seq_along(vr_levels_all)
      )
    )

  group_bracket_df_detailed <- group_bracket_df %>%
    dplyr::mutate(
      x = bracket_x,
      x_inner = bracket_x + bracket_arm
    )

  p_msa <- ggplot() +
    geom_segment(
      data = group_bracket_df_detailed,
      aes(x = x, xend = x, y = y_start, yend = y_end),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.75,
      lineend = "round"
    ) +
    geom_segment(
      data = group_bracket_df_detailed,
      aes(x = x, xend = x_inner, y = y_start, yend = y_start),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.75,
      lineend = "round"
    ) +
    geom_segment(
      data = group_bracket_df_detailed,
      aes(x = x, xend = x_inner, y = y_end, yend = y_end),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.75,
      lineend = "round"
    ) +
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
      limits = c(max(detailed_backbone_df$y) + 0.5, min(species_header_df$y_header) - 0.2),
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

  plot_height <- max(500, 80 + (max(c(detailed_backbone_df$y, 1), na.rm = TRUE) * 18))

  list(
    plot = p_msa,
    plot_height = plot_height
  )
}
