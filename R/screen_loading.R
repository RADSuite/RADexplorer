loading_screen_ui <- function() {
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
      h3("Loading RADexplorer..."),
      div(class = "spinner-border", role = "status"),
      p("Performing multiple sequence alignments and building visualization...")
    )
  )
}
