make_msa_plotly <- function(taxon, varRegions,
                            RADq_path = "testdata/exampleRADq.csv") {
  library(tidyverse)
  library(plotly)
  library(ggtext)
  
  # read in RADq input
  RADq <- readr::read_csv(RADq_path, show_col_types = FALSE)
  
  # user selected variable regions
  selectedVariableRegions <- varRegions
  selected_regions_clean <- sub("regions$", "", selectedVariableRegions)
  
  #View(RADq)
  
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
  
  #### determines spacing between species based on how many gene copies there are
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
      x_one   = 1,  # one tile per facet column, x is basically just a placeholder
      species = factor(species, levels = species_levels)
    ) %>%
    left_join(copies_tbl %>% select(species, start), by = "species") %>%
    mutate(
      y = start + copy_num - 1
    )
  
  ####
  
  # one label per species block 
  y_breaks <- copies_tbl %>% select(species, y_lab, n_copies)
  
  #####################################################################################
  
  RADqtiles <- RADqtiles %>%
    mutate(
      seq_id_local = factor(gsub("^V[0-9]+", "", seq_id))
    )
  
  # reorder copies within each species, groups like copies together
  RADqtiles <- RADqtiles %>%
    group_by(species, variable_region_clean) %>%
    add_count(seq_id_local, name = "seq_n") %>%
    arrange(desc(seq_n), seq_id_local, copy_num, .by_group = TRUE) %>%
    mutate(
      copy_num = row_number(),           
      start = as.numeric(start),
      y = start + as.numeric(copy_num) - 1
    ) %>%
    ungroup() %>%
    select(-seq_n)
  
  #####################################################################################
  # determining coordinates for bracket on the left
  
  tile_center <- 1
  tile_width  <- 0.95
  
  # find the leftmost facet 
  first_facet <- RADqtiles %>%
    distinct(variable_region_clean) %>%
    arrange(variable_region_clean) %>%
    pull(variable_region_clean) %>%
    .[1]
  
  brackets_one <- copies_tbl %>%
    transmute(
      ymin = start,
      ymax = end,
      x    = tile_center - tile_width/2 - 0.10,
      tick = 0.05,
      variable_region_clean = first_facet
    )
  
  #####################################################################################
  
  p_msa <- ggplot(RADqtiles, aes(x = x_one, y = y)) +
    geom_tile(aes(fill = seq_id_local), color = "black", width = 0.95, height = 0.95) +
    facet_grid(~ variable_region_clean, scales = "free_x") +
    scale_x_continuous(breaks = 1, labels = "") +
    scale_y_continuous(
      breaks = y_breaks$y_lab,
      labels = paste0(
        "<span style='font-size:10pt; line-height:1.1; font-weight:500;'>", y_breaks$species, "</span>",
        "<br><span style='font-size:6pt; line-height:1.1;'>", y_breaks$n_copies, " 16S gene copies</span>"
      ),
      trans = "reverse"
    ) +
    # bracket vertical 
    geom_segment(
      data = brackets_one,
      aes(x = x, xend = x, y = ymin, yend = ymax),
      inherit.aes = FALSE
    ) +
    # bracket top 
    geom_segment(
      data = brackets_one,
      aes(x = x, xend = x + tick, y = ymax, yend = ymax),
      inherit.aes = FALSE
    ) +
    # bracket bottom 
    geom_segment(
      data = brackets_one,
      aes(x = x, xend = x + tick, y = ymin, yend = ymin),
      inherit.aes = FALSE
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "right",
      axis.text.y = ggtext::element_markdown(),
      strip.text = element_text(size = 12)
    )
  
  #####################################################################################
  # convert to plotly 
  
  p_plotly <- ggplotly(
    p_msa,
    tooltip = c("y", "variable_region_clean", "copy_num", "seq_id")
  ) %>%
    layout(margin = list(l = 125, r = 30, t = 50, b = 40))
  
  #####################################################################################
  # fix formatting on facet titles, positions, etc
  
  facet_levels <- RADqtiles %>%
    distinct(variable_region_clean) %>%
    arrange(variable_region_clean) %>%
    pull(variable_region_clean)
  
  n_facets <- length(facet_levels)
  
  gap_dom <- 0.001  # horizontal gap between facets
  panel_w <- (1 - gap_dom * (n_facets - 1)) / n_facets
  
  # enforce same width for each facet
  for (i in seq_len(n_facets)) {
    ax <- if (i == 1) "xaxis" else paste0("xaxis", i)
    left  <- (i - 1) * (panel_w + gap_dom)
    right <- left + panel_w
    p_plotly$x$layout[[ax]]$domain <- c(left, right)
  }
  
  # recenter facet titles
  mids <- (seq_len(n_facets) - 1) * (panel_w + gap_dom) + panel_w / 2
  anns <- p_plotly$x$layout$annotations
  
  for (i in seq_len(n_facets)) {
    idx <- which(vapply(anns, function(a) isTRUE(a$text == facet_levels[i]), logical(1)))
    if (length(idx) == 1) {
      anns[[idx]]$xref <- "paper"
      anns[[idx]]$x <- mids[i]
      anns[[idx]]$xanchor <- "center"
    }
  }
  
  p_plotly$x$layout$annotations <- anns
  
  #####################################################################################
  # final output
  
  p_plotly
}