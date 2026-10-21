#' Save model code to a markdown file
#' 
#' @param model nlmixr2 model object (`rxUi`), or model code as a character
#'   vector (e.g. NONMEM control stream lines).
#' @param path path to .md file to save model code to
#' 
#' @returns Nothing, called for its side effect of writing `path`.
#' 
#' @export
save_model_code <- function(
  model,
  path
) {
  if (inherits(model, "rxUi")) {
    model <- deparse(as.function(model))
  }
  md <- c(
    "## Model code",
    "",
    "```",
    as.character(model),
    "```"
  )

  writeLines(md, path)
}
