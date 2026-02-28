make_msa_plotly <- function(
    taxon,
    varRegions = c("V1regions","V2regions","V3regions","V4regions","V5regions","V6regions","V7regions","V8regions","V9regions"),
    RADq_path = NULL,
    unique_path = NULL,
    groupings_path = NULL,
    highlight_unique = FALSE,
    detailed = FALSE,
    package = "RADexplorer"
) {

  library(tidyverse)
  library(plotly)
  library(ggtext)

  #uses test data if no data is loaded
  if (is.null(RADq_path) || is.null(unique_path)) {
    test_dir <- system.file("testdata", package = package)
    if (test_dir == "") stop("Could not find inst/testdata in installed package.")
    if (is.null(RADq_path))   RADq_path   <- file.path(test_dir, "exampleRADq.csv")
    if (is.null(unique_path)) unique_path <- file.path(test_dir, "unique.csv")
    if (is.null(groupings_path)) groupings_path <- file.path(test_dir, "examplegroups_long.csv")
  }

  # read in RADq input
  RADq   <- readr::read_csv(RADq_path, show_col_types = FALSE)
  unique <- readr::read_csv(unique_path, show_col_types = FALSE)
  groups <- readr::read_csv(groupings_path, show_col_types = FALSE)

  # user selected variable regions
  selectedVariableRegions <- varRegions
  selected_regions_clean <- sub("regions$", "", selectedVariableRegions)

  #####################################################################################
  # detailed mode data prep (RADq tiles)

  # keep only selected regions + keep only the columns we actually use for plotting
  RADqtiles <- RADq %>%
    filter(variable_region %in% selected_regions_clean) %>%
    transmute(
      species,
      variable_region_clean = variable_region,
      copy_num,
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
      x_one   = 1,  # placeholder x for each facet
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
  rect_df <- tibble()

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

    # rectangles that surround the entire species block in that facet
    tile_w <- 0.7
    rect_df <- unique_long %>%
      distinct(species, variable_region_clean) %>%
      left_join(copies_tbl %>% select(species, start, end), by = "species") %>%
      mutate(
        xmin = 1 - tile_w / 2,
        xmax = 1 + tile_w / 2,
        ymin = start - 0.475,
        ymax = end + 0.475
      ) %>%
      filter(!is.na(start), !is.na(end))

    rect_df <- rect_df %>%
      mutate(hover = paste0(
        "Taxon: ", species,
        "<br>Region: ", variable_region_clean
      ))
  }

  #####################################################################################
  # reorder copies within each species, groups like copies together

  #makes hover text display "Species copy number"
  RADqtiles <- RADqtiles %>%
    mutate(seq_id_local = factor(gsub("^V[0-9]+", "", seq_id))) %>%
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
  # determining coordinates for bracket on the left

  tile_center <- 1
  tile_width  <- 0.95

  # first find the leftmost facet
  first_facet <- RADqtiles %>%
    distinct(variable_region_clean) %>%
    arrange(variable_region_clean) %>%
    pull(variable_region_clean) %>%
    .[1]

  # then bracket coords
  brackets_one <- copies_tbl %>%
    transmute(
      ymin = start,
      ymax = end,
      x    = tile_center - tile_width/2 - 0.10,
      tick = 0.05,
      variable_region_clean = first_facet
    )

  #####################################################################################
  # build ggplot

  RADqtiles_unique <- RADqtiles %>% filter(is_unique_block)
  RADqtiles_nonunique <- RADqtiles %>% filter(!is_unique_block)

  alpha_nonunique <- if (highlight_unique) 0.5 else 1

  # base ggplot
  p_msa <- ggplot()

  if (detailed) {

    # map each variable region to a numeric x position (same idea as non detailed)
    vr_levels <- RADqtiles %>%
      distinct(variable_region_clean) %>%
      arrange(readr::parse_number(variable_region_clean), variable_region_clean) %>%
      pull(variable_region_clean)

    n_vr <- length(vr_levels)
    tile_w <- 0.7
    bracket_x <- 1 - tile_w/2 - 0.20
    brackets_one <- copies_tbl %>%
      transmute(
        ymin = start,
        ymax = end,
        x    = bracket_x,
        tick = 0.05
      )

    RADqtiles_nonunique <- RADqtiles_nonunique %>%
      mutate(vx = match(variable_region_clean, vr_levels))

    RADqtiles_unique <- RADqtiles_unique %>%
      mutate(vx = match(variable_region_clean, vr_levels))

    # gray line segments between adjacent regions for each species block
    seg_df <- tidyr::expand_grid(
      y = y_breaks$y_lab,
      i = seq_len(n_vr - 1)
    ) %>%
      transmute(y, x = i + tile_w/2, xend = i + 1 - tile_w/2)

    # gray caps extending left of first region and right of last region
    cap_df <- tidyr::expand_grid(
      y = y_breaks$y_lab,
      side = c("L", "R")
    ) %>%
      transmute(
        y,
        x    = ifelse(side == "L", 0.5, n_vr + tile_w/2),
        xend = ifelse(side == "L", 1 - tile_w/2, n_vr + 0.5)
      )

    seg_df <- bind_rows(seg_df, cap_df)

    # backbone
    p_msa <- p_msa +
      geom_segment(
        data = seg_df,
        aes(x = x, xend = xend, y = y, yend = y),
        inherit.aes = FALSE,
        linewidth = 3,
        color = "grey80"
      )

    # add the tiles that are non unique (if they exist)
    if (nrow(RADqtiles_nonunique) > 0) {
      p_msa <- p_msa +
        geom_tile(
          data = RADqtiles_nonunique,
          aes(x = vx, y = y, fill = seq_id_local, text = hover_text),
          alpha = alpha_nonunique,
          color = "black", width = tile_w, height = 0.95
        )
    }

    # add the tiles that are unique (if they exist)
    if (nrow(RADqtiles_unique) > 0) {
      p_msa <- p_msa +
        geom_tile(
          data = RADqtiles_unique,
          aes(x = vx, y = y, fill = seq_id_local, text = hover_text),
          alpha = 1,
          color = "black", width = tile_w, height = 0.95
        )
    }

    # formatting + brackets
    p_msa <- p_msa +
      scale_x_continuous(
        breaks = seq_len(n_vr),
        labels = vr_levels,
        position = "top",
        limits = c(0.3, n_vr + 0.5),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = y_breaks$y_lab,
        labels = paste0(
          "<span style='font-size:10pt; line-height:1.1; font-weight:500;'>", y_breaks$species, "</span>",
          "<br><span style='font-size:6pt; line-height:1.1;'>", y_breaks$n_copies, " 16S gene copies</span>"
        ),
        trans = "reverse"
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.y = ggtext::element_markdown(),
        strip.text = element_text(size = 12)
      )

    # temporarily turned off the brackets on the left
    if (FALSE) {
      p_msa <- p_msa +
        geom_segment(data = brackets_one, aes(x = x, xend = x, y = ymin, yend = ymax), inherit.aes = FALSE) +
        geom_segment(data = brackets_one, aes(x = x, xend = x + tick, y = ymax, yend = ymax), inherit.aes = FALSE) +
        geom_segment(data = brackets_one, aes(x = x, xend = x + tick, y = ymin, yend = ymin), inherit.aes = FALSE)
    }

  } else {

    groups_plot <- groups %>%
      mutate(
        vregion = factor(vregion, levels = unique(vregion[order(parse_number(vregion))])),
        taxa = as.character(taxa),
        group = factor(group)
      )

    taxa_levels <- sort(unique(groups_plot$taxa))
    y_map <- tibble(taxa = taxa_levels, y = seq_along(taxa_levels) + (seq_along(taxa_levels) - 1) * gap)

    groups_plot <- left_join(groups_plot, y_map, by = "taxa")

    n_vr <- nlevels(groups_plot$vregion)
    tile_w <- 0.7

    seg_df <- expand_grid(y = y_map$y, i = seq_len(n_vr - 1)) %>%
      transmute(y, x = i + tile_w/2, xend = i + 1 - tile_w/2)

    cap_df <- expand_grid(y = y_map$y, side = c("L", "R")) %>%
      transmute(
        y,
        x    = ifelse(side == "L", 0.5, n_vr + tile_w/2),
        xend = ifelse(side == "L", 1 - tile_w/2, n_vr + 0.5)
      )

    seg_df <- bind_rows(seg_df, cap_df)

    p_msa <- ggplot() +
      geom_tile(
        data = groups_plot,
        aes(x = as.numeric(vregion), y = y, fill = group),
        width = tile_w, height = 1.5, color = "black"
      ) +
      geom_segment(
        data = seg_df,
        aes(x = x, xend = xend, y = y, yend = y),
        linewidth = 3, color = "grey80"
      ) +
      scale_x_continuous(
        breaks = seq_len(n_vr),
        labels = levels(groups_plot$vregion),
        position = "top",
        limits = c(0.3, n_vr + 0.5),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = y_map$y,
        labels = paste0("<span style='font-size:10pt; line-height:1.1; font-weight:500;'>", rev(taxa_levels), "</span>")
      ) +
      theme_minimal() +
      theme(legend.position = "none", panel.grid = element_blank()) +
      labs(x = NULL, y = NULL)
  }

  # add outline boxes for unique regions if toggled
  ##### CURRENTLY DISABLED - BROKEN WITH LAST CHANGES #######
  if (highlight_unique && nrow(rect_df) > 0 && FALSE) {
    p_msa <- p_msa +
      geom_rect(
        data = rect_df,
        aes(
          xmin = xmin, xmax = xmax,
          ymin = ymin, ymax = ymax,
          text = hover
        ),
        inherit.aes = FALSE,
        color = "black",
        fill = NA,
        linewidth = .6
      )
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






make_msa_plotly()





