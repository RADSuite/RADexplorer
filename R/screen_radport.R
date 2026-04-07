#' Build the RADport pipeline instructions screen UI
#'
#' @return A bslib page_fillable UI object containing the step-by-step RADport pipeline instructions with copyable code blocks.
radport_screen_ui <- function() {
  bslib::page_fillable(
    title = "Metascope Integration and Instructions",
    radport_styles(),
    radport_scripts(),
    htmltools::div(class = "radport-wrap",
                   radport_header(),
                   htmltools::p(class = "step-desc", style = "font-size:15px;",
                                "Follow the steps below to integrate with the MetaScope pipeline. Copy each block and run it in your R console."),
                   htmltools::hr(class = "divider"),
                   step_section(1, "Load MetaScope", "Load the MetaScope package before running anything else.",
                                code_block("code1", CODE_1())),
                   htmltools::hr(class = "divider"),
                   step_section(2, "Set Up Reference Database", "Choose one option below. You only need to run one.",
                                opt_row("optTab1", "Option 1: Full RADlib", 1, "optTab2", "Option 2: Partial RADlib", 2),
                                htmltools::div(id = "opt1", class = "opt-panel",
                                               htmltools::p(class = "opt-note", "Downloads the full RADlib reference database bundled with RADalign."),
                                               code_block("code2a", CODE_2A())),
                                htmltools::div(id = "opt2", class = "opt-panel",
                                               htmltools::p(class = "opt-note", "Click Download to fetch the partial RADlib. To save it somewhere other than your working directory, enter a path first."),
                                               shiny::div(style = "display:flex; gap:12px; align-items:flex-end;",
                                                          shiny::textInput("filepath", label = NULL, value = ""),
                                                          shiny::actionButton("inputFilepath", "Download", style = "height:34px; padding:0 12px; margin-bottom:17px;")),
                                               htmltools::p(class = "opt-note", "After clicking download, wait a moment for the code to copy to appear below:"),
                                               shiny::uiOutput("code2b_block"))),
                   htmltools::hr(class = "divider"),
                   step_section(3, "Load Sample Data",
                                "Point to your .fastq file. For example data, download D1_16dnajoin.fastq from https://doi.org/10.5061/dryad.d41v4.",
                                code_block("code3", CODE_3())),
                   htmltools::hr(class = "divider"),
                   step_section(4, "Prepare Output Folders", "Create folders for the Bowtie index and alignment output.",
                                code_block("code4", CODE_4())),
                   htmltools::hr(class = "divider"),
                   step_section(5, "Build Bowtie Index", "Index the reference database. This may take a while but only needs to be run once.",
                                code_block("code5", CODE_5())),
                   htmltools::hr(class = "divider"),
                   step_section(6, "Align Sequences", "Align your sample reads against the reference index.",
                                code_block("code6", CODE_6())),
                   htmltools::hr(class = "divider"),
                   step_section(7, "Generate Bam File", "",
                                code_block("code7", CODE_7())),
                   htmltools::hr(class = "divider"),
                   step_section(8, "Output Table", "Choose one option below. You only need to run one.",
                                opt_row("optTab3", "Option 1: Species names", 3, "optTab4", "Option 2: TaxID", 4),
                                htmltools::div(id = "opt3", class = "opt-panel",
                                               htmltools::p(class = "opt-note", "Use species names and read counts."),
                                               code_block("code8a", CODE_8A())),
                                htmltools::div(id = "opt4", class = "opt-panel",
                                               htmltools::p(class = "opt-note", "Use TaxID and read counts."),
                                               code_block("code8b", CODE_8B()))),
                   htmltools::hr(class = "divider"),
                   htmltools::div(class = "done-box",
                                  "All done! Your read count table is ready. Return to the menu or explore your results further.")
    )
  )
}


# helpers
radport_header <- function() {
  htmltools::div(style = "display:flex; align-items:center; margin-bottom:32px;",
                 htmltools::h3("Metascope Integration", style = "margin:0;"),
                 shiny::actionButton("backToMenu", "Back", style = "margin-left:auto;"))
}

opt_row <- function(id1, label1, n1, id2, label2, n2) {
  htmltools::div(style = "margin-bottom:12px;",
                 htmltools::tags$button(label1, id = id1, class = "opt-tab", onclick = paste0("toggleOpt(", n1, ")")),
                 htmltools::tags$button(label2, id = id2, class = "opt-tab", onclick = paste0("toggleOpt(", n2, ")")))
}

step_section <- function(n, title, desc, ...) {
  htmltools::div(class = "step-section",
                 htmltools::div(class = "step-label", paste("Step", n)),
                 htmltools::h5(class = "step-title", title),
                 htmltools::p(class = "step-desc", desc),
                 ...)
}

code_block <- function(id, code) {
  htmltools::div(style = "position:relative;",
                 htmltools::tags$button("Copy", class = "copy-btn", `data-target` = id, onclick = paste0("copyCode('", id, "')")),
                 htmltools::tags$pre(id = id, class = "code-block",
                                     htmltools::tags$code(htmltools::HTML(paste0("\n", code)))))
}


# styles
radport_styles <- function() {
  htmltools::tags$style("
    .radport-wrap { max-width:820px; margin:0 auto; padding:48px 24px 100px 24px; }
    .step-label   { font-size:11px; font-weight:600; letter-spacing:0.08em; text-transform:uppercase; color:#888; }
    .step-title   { font-size:18px; font-weight:600; margin:4px 0 8px 0; }
    .step-desc    { font-size:14px; color:#444; line-height:1.6; margin-bottom:12px; }
    .step-section { margin-bottom:36px; }
    .code-block   { background:#f6f6f6; border:1px solid #e0e0e0; border-radius:8px; padding:14px 16px;
                    font-family:monospace; font-size:13px; overflow-x:auto; margin:0; }
    .copy-btn     { position:absolute; top:8px; right:10px; font-size:11px; padding:3px 10px;
                    border:1px solid #ccc; border-radius:4px; background:white; cursor:pointer; z-index:1; }
    .copy-btn:hover { background:#f0f0f0; }
    .opt-tab      { font-size:12px; padding:4px 14px; border-radius:20px; border:1px solid #0d6efd;
                    background:white; color:#0d6efd; cursor:pointer; margin-right:8px; }
    .opt-tab.active { background:#0d6efd; color:white; }
    .opt-panel    { display:none; }
    .opt-panel.active { display:block; }
    .opt-note     { font-size:13px; color:#555; margin-bottom:8px; }
    .divider      { border:none; border-top:1px solid #eee; margin:0 0 36px 0; }
    .done-box     { background:#f0faf4; border:1px solid #b2dfcc; border-radius:8px;
                    padding:16px 20px; font-size:14px; color:#2a6049; }
  ")
}

radport_scripts <- function() {
  htmltools::tags$script(htmltools::HTML("
    function copyCode(id) {
      var code = document.querySelector('#' + id + ' code');
      var text = code ? code.innerText.trim() : '';
      navigator.clipboard.writeText(text).then(function() {
        document.querySelectorAll('.copy-btn').forEach(function(b) {
          if (b.getAttribute('data-target') === id) {
            b.innerText = 'Copied!';
            setTimeout(function(){ b.innerText = 'Copy'; }, 1500);
          }
        });
      });
    }
    function toggleOpt(opt) {
      [1,2,3,4].forEach(function(i) {
        var panel = document.getElementById('opt'+i);
        var tab   = document.getElementById('optTab'+i);
        if (panel) panel.classList.toggle('active', i===opt);
        if (tab)   tab.classList.toggle('active',   i===opt);
      });
    }
  "))
}


# code strings (instructions for users)
CODE_1  <- function() 'library("MetaScope")'

CODE_2A <- function() {
  'rad_lib_file <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
dir.create("refdata")
file.copy(rad_lib_file, "refdata")
ref <- "refdata"'
}

CODE_3  <- function() 'data <- "path/to/your/file/data_file.fastq"'

CODE_4  <- function() {
  'indices <- tempfile()
dir.create(indices)
dir.create("out")'
}

CODE_5  <- function() {
  'mk_bowtie_index(
  ref_dir = ref,
  lib_dir = indices,
  lib_name = "target",
  overwrite = TRUE)'
}

CODE_6  <- function() {
  'target_map <- align_target_bowtie(
  data,
  lib_dir = indices,
  libs = "target",
  align_dir = "out",
  align_file = "bowtie_target",
  overwrite = TRUE)'
}

CODE_7  <- function() {
  'bamFile <- Rsamtools::BamFile(target_map)
param <- Rsamtools::ScanBamParam(
  flag = Rsamtools::scanBamFlag(isSecondaryAlignment = FALSE),
  what = c("flag", "rname")
)
aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname'
}

CODE_8A <- function() {
  'genome_name_all <- get_species_list(accession_all) # RADalign function
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)
knitr::kable(read_count_table[1:10], col.names = c("Genome Assigned", "Read Count"))'
}

CODE_8B <- function() {
  'taxa_id_all <- get_taxa_ids(accession_all) # RADalign function
read_count_table <- sort(table(taxa_id_all), decreasing = TRUE)
knitr::kable(read_count_table[1:10], col.names = c("Genome Assigned", "Read Count"))'
}
