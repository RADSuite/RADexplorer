CODE_1 <- function() {
  'library("MetaScope")'
}

CODE_2A <- function() {
  'rad_lib_file <- system.file("extdata", "RADlib16S.fa", package = "RADalign")
dir.create("refdata")
file.copy(rad_lib_file, "refdata")
ref <- "refdata"'
}

CODE_2B <- function() {
  'ref <- "your/RADdownload/file/path/here"'
}

CODE_3 <- function() {
  'data <- "path/to/your/file/data_file.fastq"'
}

CODE_4 <- function() {
  'indices <- tempfile()
dir.create(indices)
dir.create("out")'
}

CODE_5 <- function() {
  'mk_bowtie_index(
  ref_dir = ref,
  lib_dir = indices,
  lib_name = "target",
  overwrite = TRUE)'
}

CODE_6 <- function() {
  'target_map <- align_target_bowtie(
  data,
  lib_dir = indices,
  libs = "target",
  align_dir = "out",
  align_file = "bowtie_target",
  overwrite = TRUE)'
}

CODE_7 <- function() {
  'bamFile <- Rsamtools::BamFile(target_map)
param <- Rsamtools::ScanBamParam(
  flag = Rsamtools::scanBamFlag(isSecondaryAlignment = FALSE),
  what = c("flag", "rname")
)
aln <- Rsamtools::scanBam(bamFile, param = param)
accession_all <- aln[[1]]$rname
genome_name_all <- get_species_list(accession_all)
read_count_table <- sort(table(genome_name_all), decreasing = TRUE)
knitr::kable(
  read_count_table[1:10],
  col.names = c("Genome Assigned", "Read Count"))'
}

code_block <- function(id, code) {
  htmltools::div(style = "position:relative;",
                 htmltools::tags$button("Copy", class = "copy-btn", `data-target` = id, onclick = paste0("copyCode('", id, "')")),
                 htmltools::tags$pre(id = id, class = "code-block",
                                     htmltools::tags$code(htmltools::HTML(paste0("\n", code)))
                 )
  )
}

step_section <- function(n, title, desc, ...) {
  htmltools::div(class = "step-section",
                 htmltools::div(class = "step-label", paste("Step", n)),
                 htmltools::h5(class = "step-title", title),
                 htmltools::p(class = "step-desc", desc),
                 ...
  )
}

radport_screen_ui <- function() {
  bslib::page_fillable(
    title = "RADport",
    htmltools::tags$style("
      .radport-wrap { max-width:820px; margin:0 auto; padding:48px 24px 100px 24px; }
      .step-label { font-size:11px; font-weight:600; letter-spacing:0.08em; text-transform:uppercase; color:#888; }
      .step-title { font-size:18px; font-weight:600; margin:4px 0 8px 0; }
      .step-desc  { font-size:14px; color:#444; line-height:1.6; margin-bottom:12px; }
      .step-section { margin-bottom:36px; }
      .code-block { background:#f6f6f6; border:1px solid #e0e0e0; border-radius:8px; padding:14px 16px;
                    font-family:monospace; font-size:13px; overflow-x:auto; margin:0; }
      .copy-btn   { position:absolute; top:8px; right:10px; font-size:11px; padding:3px 10px;
                    border:1px solid #ccc; border-radius:4px; background:white; cursor:pointer; z-index:1; }
      .copy-btn:hover { background:#f0f0f0; }
      .opt-tab    { font-size:12px; padding:4px 14px; border-radius:20px; border:1px solid #0d6efd;
                    background:white; color:#0d6efd; cursor:pointer; margin-right:8px; }
      .opt-tab.active { background:#0d6efd; color:white; }
      .opt-panel  { display:none; } .opt-panel.active { display:block; }
      .opt-note   { font-size:13px; color:#555; margin-bottom:8px; }
      .divider    { border:none; border-top:1px solid #eee; margin:0 0 36px 0; }
      .done-box   { background:#f0faf4; border:1px solid #b2dfcc; border-radius:8px;
                    padding:16px 20px; font-size:14px; color:#2a6049; }
    "),
    htmltools::tags$script(htmltools::HTML("
      function copyCode(id) {
        navigator.clipboard.writeText(document.getElementById(id).innerText).then(function() {
          var btns = document.querySelectorAll('.copy-btn');
          btns.forEach(function(b) {
            if (b.getAttribute('data-target') === id) {
              b.innerText = 'Copied!';
              setTimeout(function(){ b.innerText = 'Copy'; }, 1500);
            }
          });
        });
      }
      function toggleOpt(opt) {
        [1,2].forEach(function(i) {
          document.getElementById('opt'+i).classList.toggle('active', i===opt);
          document.getElementById('optTab'+i).classList.toggle('active', i===opt);
        });
      }
      document.addEventListener('DOMContentLoaded', function() { toggleOpt(1); });
    ")),

    htmltools::div(class = "radport-wrap",

                   htmltools::div(style = "display:flex; align-items:center; margin-bottom:32px;",
                                  htmltools::h3("RADport", style = "margin:0;"),
                                  shiny::actionButton("backToMenu", "Back", style = "margin-left:auto;")
                   ),

                   htmltools::p(class = "step-desc", style = "font-size:15px;",
                                "Follow the steps below to run the RADport pipeline. Copy each block and run it in your R console."),

                   htmltools::hr(class = "divider"),

                   step_section(1, "Load MetaScope", "Load the MetaScope package before running anything else.",
                                code_block("code1", CODE_1())
                   ),

                   htmltools::hr(class = "divider"),

                   step_section(2, "Set Up Reference Database", "Choose one option below. You only need to run one.",
                                htmltools::div(
                                  htmltools::tags$button("Option 1: Full RADlib",    id = "optTab1", class = "opt-tab active", onclick = "toggleOpt(1)"),
                                  htmltools::tags$button("Option 2: Partial RADlib", id = "optTab2", class = "opt-tab",        onclick = "toggleOpt(2)")
                                ),
                                htmltools::div(id = "opt1", class = "opt-panel",
                                               htmltools::p(class = "opt-note", "Downloads the full RADlib reference database bundled with RADalign."),
                                               code_block("code2a", CODE_2A())
                                ),
                                htmltools::div(id = "opt2", class = "opt-panel",
                                               htmltools::p(class = "opt-note", "Use a partial RADlib aligned only to your species of interest. Replace the path below."),
                                               code_block("code2b", CODE_2B())
                                )
                   ),

                   htmltools::hr(class = "divider"),

                   step_section(3, "Load Sample Data",
                                "Point to your .fastq sample file. To use example data, download D1_16dnajoin.fastq from https://doi.org/10.5061/dryad.d41v4 and update the path below.",
                                code_block("code3", CODE_3())
                   ),

                   htmltools::hr(class = "divider"),

                   step_section(4, "Prepare Output Folders", "Create the folders needed for the Bowtie index and alignment output.",
                                code_block("code4", CODE_4())
                   ),

                   htmltools::hr(class = "divider"),

                   step_section(5, "Build Bowtie Index", "Index the reference database so Bowtie can align against it.",
                                code_block("code5", CODE_5())
                   ),

                   htmltools::hr(class = "divider"),

                   step_section(6, "Align Sequences", "Align your sample reads against the reference index.",
                                code_block("code6", CODE_6())
                   ),

                   htmltools::hr(class = "divider"),

                   step_section(7, "View Results", "Extract read counts per organism and display the top results.",
                                code_block("code7", CODE_7())
                   ),

                   htmltools::hr(class = "divider"),

                   htmltools::div(class = "done-box",
                                  "All done! Your read count table is ready. Return to the menu or explore your results further.")
    )
  )
}
