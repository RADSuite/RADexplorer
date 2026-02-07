make_msa_plotly <- function(taxon, varRegions,
                            RADq_path = "testdata/example_RADx_input.csv",
                            RADx_occ_path = "testdata/example_RADx_occurrences.csv") {
  library(tidyverse)
  library(plotly)
  
  # this parser turns a text list in a cell into a real R list - this will depend on what the file actually looks like
  parser <- function(x) {
    if (is.na(x) || x == "") return(character(0))
    x2 <- gsub('\\\\\"', '"', x)
    tmp <- readr::read_csv(I(x2), col_names = FALSE, show_col_types = FALSE)
    as.character(unlist(tmp[1, ], use.names = FALSE))
  }
  
  # inputs
  RADq <- readr::read_csv(RADq_path, show_col_types = FALSE)
  RADx_occ <- readr::read_csv(RADx_occ_path, show_col_types = FALSE)
  
  # which variable regions you want to plot - user selects these on RShiny
  selectedVariableRegions <- c(
    "V1regions","V2regions","V3regions","V4regions","V5regions","V6regions","V7regions","V8regions","V9regions"
  )
  
  # keep only what we need from RADq
  RADqfiltered <- RADq %>% select(species, any_of(selectedVariableRegions))
  
  # go long, parse lists, go longer so every copy is one row
  RADqlonger <- RADqfiltered %>%
    pivot_longer(
      cols = -species,
      names_to = "variable_region",
      values_to = "sequences"
    ) %>%
    mutate(
      variable_region_clean = sub("regions$", "", variable_region),
      sequences = map(sequences, parser)
    ) %>%
    unnest_wider(sequences, names_sep = "_") %>%
    pivot_longer(
      cols = starts_with("sequences_"),
      names_to = "copy_id",
      values_to = "sequence"
    ) %>%
    mutate(
      copy_num = readr::parse_number(copy_id)
    ) %>%
    filter(!is.na(sequence) & sequence != "")
  
  
  #####################################################################################
  
  # this designs the stacked tile setup, calculates y coordinates based on number of copies
  gap <- .5  # space between species
  
  species_levels <- RADqlonger %>%
    distinct(species) %>%
    arrange(species) %>%
    pull(species)
  
  # count how many copies each species actually has (so no weird empty space inside blocks)
  copies_tbl <- RADqlonger %>%
    distinct(species, copy_num) %>%
    count(species, name = "n_copies") %>%
    mutate(species = factor(species, levels = species_levels)) %>%
    arrange(species) %>%
    mutate(
      start = lag(cumsum(n_copies + gap), default = 0) + 1,
      end   = start + n_copies - 1,
      y_lab = (start + end) / 2
    )
  
  
  RADqtiles <- RADqlonger %>%
    mutate(
      x_one   = 1,
      species = factor(species, levels = species_levels)
    ) %>%
    left_join(copies_tbl %>% select(species, start), by = "species") %>%
    mutate(
      y = start + copy_num - 1
    )
  
  # y axis labels - show species name only once per group of gene copies
  y_breaks <- copies_tbl %>% select(species, y_lab, n_copies)
  
  #####################################################################################
  
  # map (region, sequence) -> seq_id from the RADx occurrences file
  # this is what we use to color the tiles
  seq_map <- RADx_occ %>%
    transmute(
      variable_region_clean = variable_region,
      sequence,
      seq_id
    ) %>%
    distinct(variable_region_clean, sequence, seq_id) %>%
    mutate(seq_id = factor(seq_id))
  
  RADqtiles <- RADqtiles %>%
    left_join(seq_map, by = c("variable_region_clean", "sequence"))
  
  # make seq_id local within each variable region
  # this is to make colors reset inside each variable region - is that what we want?
  RADqtiles <- RADqtiles %>%
    group_by(variable_region_clean) %>%
    mutate(
      seq_id_local = factor(dense_rank(seq_id))
    ) %>%
    ungroup()
  
  #View(RADqtiles)
  
  # order the gene copies so that like colors are grouped together
  RADqtiles <- RADqtiles %>%
    group_by(species, variable_region_clean) %>%
    add_count(sequence, name = "seq_n") %>%
    arrange(desc(seq_n), sequence, copy_num, .by_group = TRUE) %>%
    mutate(
      copy_num = row_number(),
      start = as.numeric(start),
      y = start + as.numeric(copy_num) - 1
    ) %>%
    ungroup() %>%
    select(-seq_n)
  
  #####################################################################################
  
  # bracket coordinate calculation
  tile_center <- 1
  tile_width  <- 0.95
  
  first_facet <- RADqtiles %>%
    distinct(variable_region_clean) %>%
    arrange(variable_region_clean) %>%
    pull(variable_region_clean) %>%
    .[1]
  
  brackets_one <- copies_tbl %>%
    transmute(
      ymin = start,
      ymax = end,
      x    = tile_center - tile_width/2 - 0.10,  # gap left of tile edge
      tick = 0.05,
      variable_region_clean = first_facet
    )
  
  #####################################################################################
  
  View(y_breaks)
  
  # tile plot of selected variable regions by species.
  # segments are the bracket on the leftmost variable region to show species 
  p_msa <- ggplot(RADqtiles, aes(x = x_one, y = y)) +
    geom_tile(aes(fill = seq_id_local), color = "black", width = 0.95, height = 0.95) +
    facet_grid(~ variable_region_clean, scales = "free_x") +   # remove space = "free_x"
    scale_x_continuous(breaks = 1, labels = "") +
    scale_y_continuous(
      breaks = y_breaks$y_lab,
      labels = paste0(y_breaks$species, "\n", y_breaks$n_copies, " 16S gene copies"),
      trans = "reverse",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    # the geom_segments add the bracket for each species
    geom_segment(
      data = brackets_one,
      aes(x = x, xend = x, y = ymin, yend = ymax),
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = brackets_one,
      aes(x = x, xend = x + tick, y = ymax, yend = ymax),
      inherit.aes = FALSE
    ) +
    geom_segment(
      data = brackets_one,
      aes(x = x, xend = x + tick, y = ymin, yend = ymin),
      inherit.aes = FALSE
    ) +
    labs(x = NULL, y = "Species") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = "none")
  
  p_plotly <- ggplotly(p_msa, tooltip = c("y", "variable_region_clean", "copy_num", "sequence"))
  
  
  ##### this is formatting to make sure all the different facets are the same width
  facet_levels <- RADqtiles %>%
    distinct(variable_region_clean) %>%
    arrange(variable_region_clean) %>%
    pull(variable_region_clean)
  
  n_facets <- length(facet_levels)
  
  gap_dom <- 0.01  # gap between facet panels
  panel_w <- (1 - gap_dom * (n_facets - 1)) / n_facets
  
  for (i in seq_len(n_facets)) {
    ax <- if (i == 1) "xaxis" else paste0("xaxis", i)
    left  <- (i - 1) * (panel_w + gap_dom)
    right <- left + panel_w
    
    if (!is.null(p_plotly$x$layout[[ax]])) {
      p_plotly$x$layout[[ax]]$domain <- c(left, right)
    }
  }
  ######
  
  p_plotly
}