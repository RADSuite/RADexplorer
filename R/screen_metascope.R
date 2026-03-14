metascope_screen_ui <- function(genus, species) {
  # Metascope download menu
  page_fillable(
    title = "RADport to Metascope",
    fillable = TRUE,
    div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:50px; padding-bottom:50px; height:100vh; overflow:hidden;",
      div(
        style = "display:flex; gap:24px; width:min(1400px, 95vw); height:calc(100vh - 150px); overflow:hidden; align-items:stretch;",
        card(
          style = "flex:1 1 0; height:100%; overflow:hidden;",
          card_body(
            style = "height:100%; overflow-y:auto; overflow-x:hidden; min-height:0; line-height:1.5;",

            h4("MetaScope Instructions"),

            h5("STEP 1 - MetaDemultiplex"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
barcodePath <- "your/path/here/barcode_file.fastq"
indexPath <- "your/path/here/index_file.fastq"
readPath <- "your/path/here/read_file.fastq"

demult <-
  meta_demultiplex(barcodePath,
                   indexPath,
                   readPath,
                   rcBarcodes = FALSE,
                   hammingDist = 2,
                   location = tempfile())
demult        ')
            ),
            p("Note: Only complete this step if your reads require demultiplexing, otherwise skip it."),

            h5("STEP 2 - MetaRef: Creating a Taxonomy Database"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
tmp_accession <- file.path("your_folder", "MetaScope_accessions_db.sqlite")
                        ')
            ),
            p("Your folder:"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
/Users/user/Downloads/RADdownloads_05032026_204741_KYvVgrko
                        ')
            ),

            h5("STEP 3 - MetaRef: Downloading target genomes"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
target_ref_temp <- file.path("your_folder", "Metascope_reference_dir")
                        ')
            ),
            p("Your folder:"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
/Users/user/Downloads/RADdownloads_05032026_204741_KYvVgrko
                        ')
            ),

            h5("STEP 4 - MetaRef: Downloading filter genomes"),
            p("IF filter genomes are needed:"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
<SELECT SPECIES>

<DOWNLOAD FILTER DATASET>
                        ')
            ),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
filter_ref_temp <- file.path("your_folder", "Metascope_filter_dir")
                      ')
            ),
            p("Your folder:"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
/Users/user/Downloads/RADdownloads_05032026_204741_KYvVgrko
                        ')
            ),

            h5("STEP 5 - Creating indices using a given aligner"),
            tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              tags$code('
# Create temp directory to store the Bowtie2 indices
index_temp <- tempfile()
dir.create(index_temp)

# Create target index
mk_bowtie_index(
  ref_dir = target_ref_temp,
  lib_dir = index_temp,
  lib_name = "target",
  overwrite = TRUE
)

# Create filter index
mk_bowtie_index(
  ref_dir = filter_ref_temp,
  lib_dir = index_temp,
  lib_name = "filter",
  overwrite = TRUE
)
                      ')
            ),
          p("Note: If you ran the above lines of code correctly, target_ref_temp and filter_ref_temp will refer to your downloaded files."),

          h5("STEP 6 - MetaAlign"),
          tags$pre(
            style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
            tags$code('
# Create a temp directory to store output bam file
output_temp <- tempfile()
dir.create(output_temp)

readPath <- "your/path/here/read_file.fastq"

# Align reads to the target genomes
target_map <-
  align_target_bowtie(
    read1 = readPath,
    lib_dir = index_temp,
    libs = "target",
    align_dir = output_temp,
    align_file = "bowtie_target",
    overwrite = TRUE
  )
                      ')
          ),
          p("Note: If your reads require demultiplexing, refer to the first step above."),

          h5("STEP 7 - MetaFilter"),
          tags$pre(
            style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
            tags$code('
final_map <- filter_host_bowtie(
  reads_bam = target_map,
  lib_dir = index_temp,
  libs = "filter",
  make_bam = TRUE, # Set to true to create BAM output
  # Default is to create simplified .csv.gz output
  # The .csv.gz output is much quicker to create!
  overwrite = TRUE,
  threads = 1
)
            ')
          ),

          h5("STEP 8 - MetaID: Origin Genome Identification"),
          tags$pre(
            style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
            tags$code('
output <- metascope_id(
  final_map,
  input_type = "bam",
  # change input_type to "csv.gz" when not creating a BAM
  aligner = "bowtie2",
  num_species_plot = 0,
  accession_path = tmp_accession
)

knitr::kable(
  output,
  format = "html",
  digits = 2,
  caption = "Table of MetaScope ID results"
)
                    ')
          ),
        p("Note: If you ran the above lines of code correctly, tmp_accession will refer to your downloaded files.")
          )
        ),

    card(
      style = "flex:1 1 0; height:100%; overflow:hidden;",
      card_body(
        style = "height:100%; overflow:hidden; min-height:0;",
        div(
          style = "display:flex; flex-direction:column; gap:16px; height:100%;",
          div(
            style = "display:flex; gap:12px; width:100%;",
            h4("RADport", style = "margin:0;"),
            actionButton("backToMenu", "Back", style = "margin-left:auto;")
          ),
          div(
            h4("To create MetaScope filters, select taxa below."),
            p("If no filters are needed, leave the selection blank."),
            # genus selection
            tags$div(
              tags$label(
                `for` = "selectGenus",
                style = "display:block; margin-bottom:0px;",
                tags$div("Select genus or genera to filter:"),
                tags$div(
                  style = "font-size:13px; margin-top:0px;",
                  class = "checkbox",
                  tags$label(
                    checkboxInput(
                      "entireGenus",
                      label = HTML("<i>Filter all members of the selected genus</i>"),
                      value = FALSE
                    )
                  )
                )
              ),
              selectizeInput(
                "selectGenusFilter", label = NULL,
                choices = genus,
                multiple = TRUE,
                options = list(placeholder = "Type to search", maxOptions = 10000, openOnFocus = FALSE),
                width = "100%"
              )
            ),
            # species select - only shows up after you select a genus or genera
            conditionalPanel(
              condition = "input.selectGenusFilter && input.selectGenusFilter.length > 0",
              selectizeInput(
                "selectTaxaFilter", "Select species to filter:",
                choices = character(0),
                multiple = TRUE,
                options = list(
                  placeholder = "Type to search",
                  maxOptions = 10000,
                  closeAfterSelect = FALSE,
                  openOnFocus = FALSE
                ),
                width = "100%"
              )
            )
          ),
          div(
            style = "margin-top:auto;",
            fluidPage(
              useShinyjs(),
              div(
                style = "display:flex; gap:12px; width:100%;",
                actionButton("port", "Download", style = "flex:1;")
              )
            )
          )
        )
      )
    )
      )
    )
  )
}
