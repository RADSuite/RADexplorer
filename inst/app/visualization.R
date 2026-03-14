make_msa_plotly <- function(
    RADq,
    unique,
    varRegions = c("V1","V2","V3","V4","V5","V6","V7","V8","V9"),
    groupings_path = NULL,
    highlight_unique = FALSE,
    detailed = TRUE,
    package = "RADexplorer"
) {

  library(tidyverse)
  library(plotly)
  library(ggtext)

  # user selected variable regions
  selected_regions_clean <- varRegions

  # make summarized IDs compatible whether first column is species or taxa
  if ("species" %in% names(unique) && !"taxa" %in% names(unique)) {
    unique <- unique %>% dplyr::rename(taxa = species)
  }

  #####################################################################################
  # detailed mode data prep (RADq tiles)

  # keep only selected regions + keep only the columns we actually use for plotting
  RADqtiles <- RADq %>%
    filter(variable_region %in% selected_regions_clean) %>%
    transmute(
      species,
      variable_region_clean = variable_region,
      copy_num = as.numeric(copy_num),
      seq_id
    ) %>%
    filter(!is.na(copy_num), !is.na(seq_id))

  #####################################################################################
  # stack gene copies within each species, with gaps between species

  gap <- 1.5  # vertical space between species blocks

  # lock species ordering
  species_levels <- RADqtiles %>%
    distinct(species) %>%
    arrange(species) %>%
    pull(species)

  # determines spacing between species based on how many gene copies there are
  copies_tbl <- RADqtiles %>%
    distinct(species, copy_num) %>%
    count(species, name = "n_copies") %>%
    mutate(species = factor(species, levels = species_levels)) %>%
    arrange(species) %>%
    mutate(
      start = lag(cumsum(n_copies + gap), default = 0) + 1,
      end   = start + n_copies - 1,
      y_lab = (start + end) / 2
    )

  RADqtiles <- RADqtiles %>%
    mutate(
      species = factor(species, levels = species_levels)
    ) %>%
    left_join(copies_tbl %>% select(species, start), by = "species") %>%
    mutate(
      y = start + copy_num - 1
    )

  # one label per species block
  y_breaks <- copies_tbl %>% select(species, y_lab, n_copies)

  #####################################################################################
  # mark variable regions where unique (toggled)

  # default: nothing is unique, no rectangles
  RADqtiles <- RADqtiles %>% mutate(is_unique_block = FALSE)

  if (highlight_unique) {

    unique_long <- unique %>%
      select(taxa, any_of(selected_regions_clean)) %>%
      pivot_longer(
        cols = -taxa,
        names_to = "variable_region_clean",
        values_to = "flag"
      ) %>%
      filter(flag == 1) %>%
      transmute(
        species = taxa,
        variable_region_clean,
        unique_flag = TRUE
      )

    # tag tiles as unique by species x region
    RADqtiles <- RADqtiles %>%
      left_join(unique_long, by = c("species", "variable_region_clean")) %>%
      mutate(
        is_unique_block = coalesce(unique_flag, FALSE)
      ) %>%
      select(-unique_flag)
  }

  #####################################################################################
  # reorder copies within each species, groups like copies together

  #makes hover text display "Species copy number"
  RADqtiles <- RADqtiles %>%
    mutate(seq_id_local = factor(substring(seq_id, 3))) %>%
    group_by(species, variable_region_clean) %>%
    add_count(seq_id_local, name = "seq_n") %>%
    arrange(desc(seq_n), seq_id_local, copy_num, .by_group = TRUE) %>%
    mutate(
      copy_num = row_number(),
      y = as.numeric(start) + as.numeric(copy_num) - 1,
      hover_text = paste0(species, " copy number ", copy_num)
    ) %>%
    ungroup() %>%
    select(-seq_n)

  #####################################################################################
  # build ggplot

  RADqtiles_unique <- RADqtiles %>% filter(is_unique_block)
  RADqtiles_nonunique <- RADqtiles %>% filter(!is_unique_block)

  alpha_nonunique <- if (highlight_unique) 0.5 else 1


  vr_levels_all <- paste0("V", 1:9)
  selected_vr <- selected_regions_clean


  # base ggplot
  p_msa <- ggplot()

  # if detailed mode is turned on
  if (detailed) {

    # map each variable region to a numeric x position (same idea as non detailed)
    vr_levels <- vr_levels_all
    n_vr <- length(vr_levels)

    tile_w <- 0.7
    backbone_pad <- 0.45

    RADqtiles_nonunique <- RADqtiles_nonunique %>%
      mutate(vx = match(variable_region_clean, vr_levels))

    RADqtiles_unique <- RADqtiles_unique %>%
      mutate(vx = match(variable_region_clean, vr_levels))

    detailed_backbone_df <- RADqtiles %>%
      distinct(species, y)

    # backbone
    p_msa <- p_msa +
      geom_tile(
        data = detailed_backbone_df,
        aes(x = (n_vr + 1) / 2, y = y),
        inherit.aes = FALSE,
        fill = "grey80",
        color = NA,
        width = n_vr + 2 * backbone_pad,
        height = 0.20
      )

    # add the tiles that are non unique (if they exist)
    if (nrow(RADqtiles_nonunique) > 0) {
      p_msa <- p_msa +
        geom_tile(
          data = RADqtiles_nonunique,
          aes(x = vx, y = y, fill = seq_id_local, text = hover_text),
          alpha = alpha_nonunique,
          color = "black", width = tile_w, height = 0.75,
          linewidth = 0.35
        )
    }

    # add the tiles that are unique (if they exist)
    if (nrow(RADqtiles_unique) > 0) {
      p_msa <- p_msa +
        geom_tile(
          data = RADqtiles_unique,
          aes(x = vx, y = y, fill = seq_id_local, text = hover_text),
          alpha = 1,
          color = "black", width = tile_w, height = 0.75,
          linewidth = 0.35
        )
    }

    # formatting
    p_msa <- p_msa +
      scale_x_continuous(
        breaks = seq_len(n_vr),
        labels = vr_levels,
        position = "top",
        limits = c(0.3 - backbone_pad, n_vr + 0.5 + backbone_pad),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = y_breaks$y_lab,
        labels = paste0(
          "<span style='font-size:10pt; line-height:1.1; font-weight:750;'>", y_breaks$species, "</span>",
          "<br><span style='font-size:8pt; line-height:1.1;'>", y_breaks$n_copies, " 16S gene copies</span>"
        ),
        trans = "reverse"
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = ggtext::element_markdown(margin = margin(r = 10)),
        strip.text = element_text(size = 12)
      )

  } else {
    # if detailed mode is turned off
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
      mutate(
        group_id = match(group, unique(group))
      ) %>%
      ungroup()

    taxa_levels <- sort(unique(groups_plot$taxa))
    y_map <- tibble(taxa = taxa_levels, y = seq_along(taxa_levels) + (seq_along(taxa_levels) - 1) * gap)

    groups_plot <- left_join(groups_plot, y_map, by = "taxa")

    n_vr <- length(vr_levels_all)
    tile_w <- 0.7

    p_msa <- ggplot() +
      # backbone
      geom_tile(
        data = tibble(y = y_map$y),
        aes(x = (n_vr + 1) / 2, y = y),
        inherit.aes = FALSE,
        fill = "grey80",
        color = NA,
        width = n_vr,
        height = 0.25
      ) +
      # vregion blocks
      geom_tile(
        data = groups_plot %>% filter(vregion %in% selected_vr),
        aes(x = match(vregion, vr_levels_all), y = y, fill = factor(group_id)),
        width = tile_w, height = 1.5, color = "black",
        linewidth = 0.35
      ) +
      # x axis formatting
      scale_x_continuous(
        breaks = seq_len(n_vr),
        labels = vr_levels_all,
        position = "top",
        limits = c(0.3, n_vr + 0.5),
        expand = c(0, 0)
      ) +
      # y axis formatting
      scale_y_continuous(
        breaks = y_map$y,
        labels = paste0("<span style='font-size:10pt; line-height:1.1; font-weight:500;'>", taxa_levels, "</span>"),
        trans = "reverse"
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL)
  }


  #####################################################################################
  # convert to plotly

  p_plotly <- ggplotly(p_msa, tooltip = c("text", "seq_id")) %>%
    layout(margin = list(l = 125, r = 30, t = 50, b = 40))

  p_plotly <- p_plotly %>%
    layout(xaxis = list(side = "top"))


  #####################################################################################
  # final output

  p_plotly
}
