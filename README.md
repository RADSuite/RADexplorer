# RADexplorer

An interactive Shiny application for exploring 16S rRNA gene copy variation across microbial taxa to assist in variable region selection for amplicon sequencing experiments.

---

## What is RADexplorer?

Bacterial species carry multiple copies of the 16S rRNA gene (~5 on average, up to 15+), and those copies are often not identical. 
When planning an amplicon sequencing experiment, the variable region (V1-V9) you choose to sequence determines whether you can actually distinguish the species you care about from closely related taxa.

RADexplorer lets you select any set of species and immediately visualize how their 16S rRNA gene copies differ across all nine variable regions. 
You can easily see which V-regions produce unique sequences for your target taxa, which ones leave species indistinguishable from one another, and whether a single region is sufficient for your experimental needs.

### How it works

RADexplorer is built on two reference libraries:

- **RADlib** — a library of all 16S rRNA gene copies associated with each microbial taxon, sourced from NCBI
- **RADlibv** — RADlib cut into the nine standard variable regions

When you select species in RADexplorer, **RADalign** runs multiple sequence alignment and grouping analyses on those sequences across whichever variable regions you choose. 
RADexplorer then visualizes the results so you can interpret them interactively.

---

## Installation

RADexplorer is installed directly from GitHub. All dependencies are handled automatically.

**Step 1: Install pak** (if not already installed)
```r
install.packages("pak")
```

**Step 2: Install RADalign**
```r
pak::pak("RADSuite/RADalign")
```

**Step 3: Install RADexplorer**
```r
pak::pak("RADSuite/RADexplorer")
```

**Step 4: Load and launch**
```r
library(RADexplorer)
RADexplorer::run_app()
```

---

## How to Use

### 1. Select your species

When the app opens you will see the RADexplorer menu. Use the species picker to search for and select the taxa you want to analyze. You can select as many species as you need. Keep in mind, the more species you select, the longer it will take to load the explorer.

### 2. Choose your variable regions

In the sidebar, check the variable regions you want to explore. By default V4 is selected, since it is the most commonly sequenced region, but you can select any combination of V1 through V9.

### 3. Interpret the visualization

The main plot shows each selected species on the y axis and the variable regions on the x axis. Each tile represents a 16S rRNA gene copy for that species in that region. Tile colors designate identical sequences within a v-region.

- **A gold highlight** on a column marks the currently selected variable regions
- **A green checkmark** next to a species means it can be uniquely identified using the selected variable regions
- **A red bracket** groups species that cannot be distinguished from one another with the selected regions

### 4. Detailed view

Toggle **Detailed View** in the sidebar to switch from a summary view to a copy-level view, showing each individual gene copy as its own row per species.

### 5. V-Region labels

Toggle **V-Region Labels** to display sequence ID labels directly on each tile for closer inspection and discernment between v-region sequences.

### 6. Locate taxa

Use the **Locate Taxa** picker to search for specific taxa in the plot. Located taxa will be marked with a blue arrow on the y axis and the plot will be filtered to include only the taxa they are are grouped with under the current v-region selection.

---

## Typical Use Cases

**Which V region should I sequence to identify my target species?**
Select your taxa, try different V region combinations, and look for green checkmarks. A checkmark means that region produces a unique sequence for that species and you can uniquely identify it!

**Can I distinguish two closely related species with V4 alone?**
Select both species, check only V4, and see whether they fall into the same group (red bracket) or are independently identifiable (green checkmark).

**Another researcher used V3-V4 amplicon sequencing. Can I compare my V4 data to theirs?**
Check both V3 and V4 together and see whether the resolution changes between the two for your taxa of interest.

**How many 16S copies does my target organism carry, and are they identical?**
Switch to Detailed View to see every individual gene copy per species. Tiles of different colors in the same column indicate unique sequences for that region.

**I am writing a methods section and need to justify my V region choice.**
Select your taxa and check your chosen region. RADexplorer shows you exactly why that region resolves your targets. Feel free to cite RADexplorer as justification for your primer selection.

**I think two (or more) species in my data are collapsing into one OTU. Can I confirm that?**
Select your species of interest and your sequenced region. A red bracket means they share identical sequences there and cannot be told apart by that amplicon.

**My sequencing budget only allows for one region. Which one gives me the least grouping across my community of interest?**
Select your full taxa list and find the region with the fewest red brackets.

---

## RADport

RADexplorer includes **RADport**, a built-in pipeline guide that walks you through running your selected taxa through MetaScope for read-level taxonomic classification. Access it from the main menu after selecting your species.

---

## Dependencies

RADexplorer requires R >= 4.1. Core dependencies include:

- `shiny`, `bslib`, `shinyjs`, `shinyWidgets`
- `ggplot2`, `ggtext`, `plotly`
- `dplyr`, `tidyr`, `tibble`, `stringr`
- `Biostrings`, `msa`
- `RADalign`

All dependencies are installed automatically via `pak::pak("RADSuite/RADexplorer")`.

---

## Citation

If you use RADexplorer in your work, please cite the RADSuite package suite (citation tbd).

---

## License

MIT
