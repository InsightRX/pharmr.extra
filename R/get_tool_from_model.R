#' Get estimation/simulation engine from pharmpy model
#' 
#' @param model pharmpy model
#' 
#' @returns TODO
#' 
#' @export
get_tool_from_model <- function(model) {
  # TODO: Should add some input validation. Currently you can pass pretty much
  # anything and get "nonmem" in return, e.g., get_tool_from_model(1) works...
  tool <- "nonmem"
  if(inherits(model, "pharmpy.model.external.nlmixr.model.Model")) {
    tool <- "nlmixr"
  }
  tool
}
