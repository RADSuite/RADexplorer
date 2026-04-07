# RADexplorer

An interactive Shiny app for exploring 16S rRNA gene copy variation across microbial taxa, helping researchers choose the right variable regions for their amplicon sequencing experiments.

---

## What is RADexplorer?

Bacterial species carry multiple copies of the 16S rRNA gene (~5 on average, up to 15+), and those copies are often not identical. 
When planning an amplicon sequencing experiment, the variable region (v-region) you choose to sequence determines whether you can actually distinguish the species you care about from closely related taxa.

RADexplorer lets you select any set of species and immediately visualize how their 16S rRNA gene copies differ across all nine variable regions. 
You can easily see which v-regions (V1-V9) produce unique sequences for your target taxa, which ones leave species indistinguishable from one another, and whether a single region is enough for your experimental needs.

This matters more than most researchers expect. The most commonly used v-regions in amplicon sequencing, V3 and V4, are frequently insufficient to differentiate between closely related organisms within the same genus or species group.

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

<img width="1160" height="577" alt="taxaSelect" src="https://github.com/user-attachments/assets/e4473cba-d645-46ff-aa30-7d068d986f9e" />


### 2. Choose your variable regions

After the explorer loads, you can then select the variable regions you want to explore in the sidebar. By default V4 is selected, since it is the most commonly sequenced region, but you can select any combination of V1 through V9.

<img width="276" height="264" alt="vregions" src="https://github.com/user-attachments/assets/dc0e2100-8efa-4961-b84c-6186c22e1921" />


### 3. Interpret the visualization

The main plot shows each selected species on the y axis and the variable regions on the x axis. Each tile represents a 16S rRNA gene copy for that species in that region. Tile colors designate identical sequences within a v-region.

- **A gold highlight** on a column marks the currently selected variable regions
- **A green checkmark** next to a species means it can be uniquely identified using the selected variable regions
- **A red bracket** groups species that cannot be distinguished from one another with the selected regions

<img width="1145" height="643" alt="RADx" src="https://github.com/user-attachments/assets/b2b3f0fd-9112-4fc0-87b8-de91f3ed0eaa" />


### 4. Detailed view

Toggle **Detailed View** in the sidebar to switch from a summary view to a copy-level view, showing each individual gene copy as its own row per species. 
In general, the summary view is best for interspecies comparison and V-region selection, while the detailed view is best for studying intraspecies copy variation within a taxon.

<img width="1101" height="604" alt="RADxDetailed" src="https://github.com/user-attachments/assets/ac385cc9-b62b-4aa3-acc4-2490e8c606f8" />


### 5. V-Region labels

Toggle **V-Region Labels** to display sequence ID labels directly on each tile for closer inspection and discernment between v-region sequences.

<img width="1101" height="604" alt="vregionlabels" src="https://github.com/user-attachments/assets/d58334de-cd68-4a5a-92ea-77ae84f7d906" />


### 6. Locate taxa

Use the **Locate Taxa** picker to search for specific taxa in the plot. Located taxa will be marked with a blue arrow on the y axis and the plot will be filtered to include only the taxa they are are grouped with under the current v-region selection.

<img width="1136" height="460" alt="locateTaxa" src="https://github.com/user-attachments/assets/1d57ed6d-e862-49ff-8f83-97edec6fe4fd" />


---

## Typical Use Cases

**1) "Can I distinguish two closely related species with V4 alone?"**  
Select both species, check only V4, and see whether they fall into the same group (red bracket) or are identifiable (green checkmark).


**2) "How many 16S copies does my target organism carry, and are they identical?"**  
Switch to Detailed View to see every individual gene copy per species. Tiles of different colors in the same column indicate unique sequences for that region.


**3) "My budget only allows for sequencing one variable region. Which should I choose?"**   
Select your taxa and step through each V-region one at a time. The region that gives you the most green checkmarks across your taxa of interest is your best option.


**4) "Another researcher used V3 amplicon sequencing. Can I compare my V4 data to theirs?"**  
Select your taxa and check V3 and V4 separately. If the groupings and checkmarks change between the two selections, the two datasets are identifying your community differently and direct comparison may be difficult.


**5) "I am writing a methods section and need to justify the variable region I chose to sequence."**  
Select your taxa and check your chosen region. RADexplorer shows you exactly why that region identifies your specific targets. Feel free to cite RADexplorer as justification for your primer selection.


**6) "I think two (or more) species in my data are collapsing into one OTU. Can I confirm that?"**  
Select your species of interest and your sequenced region. A red bracket means they cannot be told apart by that amplicon and are therefore collapsing into one OTU.


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
