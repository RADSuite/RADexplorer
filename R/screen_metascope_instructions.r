metascope_instructions_ui <- function() {
  # MetaScope instructions page
  bslib::page_fillable(
    title = "RADport to MetaScope Instructions",
    fillable = TRUE,
    htmltools::div(
      style = "display:flex; align-items:flex-start; justify-content:center; padding-top:50px; padding-bottom:50px; height:100vh; overflow:hidden;",
      htmltools::div(
        style = "width:min(1100px, 95vw); height:calc(100vh - 150px); overflow:hidden;",
        bslib::card(
          style = "height:100%; overflow:hidden;",
          bslib::card_body(
            style = "height:100%; overflow-y:auto; overflow-x:hidden; min-height:0; line-height:1.5;",
            htmltools::div(
              style = "display:flex; gap:12px; width:100%; margin-bottom:16px;",
              htmltools::h4("MetaScope Instructions", style = "margin:0;"),
              shiny::actionButton("backToMetascopeDownload", "Back", style = "margin-left:auto;")
            ),

            htmltools::p(
              "The following link contains general instructions for using MetaScope. The steps below outline specific changes that must be made to the general MetaScope instructions in order to incorporate the RADport database files. "
            ),
            htmltools::tags$a(
              href = "https://wejlab.github.io/metascope-docs/articles/MetaScope_vignette.html",
              "MetaScope general instructions",
              target = "_blank"
            ),

            htmltools::h5("STEP 1 - MetaDemultiplex"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
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
            htmltools::p("Note: Only complete this step if your reads require demultiplexing, otherwise skip it."),

            htmltools::h5("STEP 2 - MetaRef: Creating a Taxonomy Database"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
tmp_accession <- file.path("your_folder", "MetaScope_accessions_db.sqlite")
                        ')
            ),
            htmltools::p("Your folder:"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
/Users/user/Downloads/RADdownloads_05032026_204741_KYvVgrko
                        ')
            ),

            htmltools::h5("STEP 3 - MetaRef: Downloading target genomes"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
target_ref_temp <- file.path("your_folder", "Metascope_reference_dir")
                        ')
            ),
            htmltools::p("Your folder:"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
/Users/user/Downloads/RADdownloads_05032026_204741_KYvVgrko
                        ')
            ),

            htmltools::h5("STEP 4 - MetaRef: Downloading filter genomes"),
            htmltools::p("IF filter genomes are needed:"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
<SELECT SPECIES>

<DOWNLOAD FILTER DATASET>
                        ')
            ),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
filter_ref_temp <- file.path("your_folder", "Metascope_filter_dir")
                      ')
            ),
            htmltools::p("Your folder:"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
/Users/user/Downloads/RADdownloads_05032026_204741_KYvVgrko
                        ')
            ),

            htmltools::h5("STEP 5 - Creating indices using a given aligner"),
            htmltools::tags$pre(
              style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
              htmltools::tags$code('
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
        htmltools::p("Note: If you ran the above lines of code correctly, target_ref_temp and filter_ref_temp will refer to your downloaded files."),

        htmltools::h5("STEP 6 - MetaAlign"),
        htmltools::tags$pre(
          style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
          htmltools::tags$code('
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
        htmltools::p("Note: If your reads require demultiplexing, refer to the first step above."),

        htmltools::h5("STEP 7 - MetaFilter"),
        htmltools::tags$pre(
          style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
          htmltools::tags$code('
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

      htmltools::h5("STEP 8 - MetaID: Origin Genome Identification"),
      htmltools::tags$pre(
        style = "white-space:pre-wrap; word-break:break-word; overflow-x:auto;",
        htmltools::tags$code('
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
  htmltools::p("Note: If you ran the above lines of code correctly, tmp_accession will refer to your downloaded files.")
          )
        )
      )
    )
  )
}
