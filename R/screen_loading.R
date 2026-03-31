loading_screen_ui <- function() {
  loading_messages <- c(
    "Parsing FASTA files...",
    "Clustering sequences...",
    "Mapping regions V1–V9...",
    "Grouping identical reads...",
    "Collapsing redundant taxa...",
    "Filtering low-quality reads...",
    "Resolving ambiguous bases...",
    "Checking alignment consistency...",
    "Reconciling duplicate IDs...",
    "Amplifying target regions...",
    "Trusting the pipeline...",
    "Feeding the cultures...",
    "Identifying taxa...",
    "Streaking agar plates...",
    "Looking for the one working pipette...",
    "Centrifuging samples...",
    "Extracting DNA...",
    "Frantically coding..."
  )

  bslib::page_fillable(
    div(
      style = "
        height:100vh;
        display:flex;
        align-items:center;
        justify-content:center;
        flex-direction:column;
        gap:16px;
      ",
      h3("Loading RADx..."),
      div(class = "spinner-border", role = "status"),
      p(sample(loading_messages, 1))
    )
  )
}
