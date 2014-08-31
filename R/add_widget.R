#' @export
add_widget <- function(name, package = name){
  tpl <- "#' Title
#'
#' Description
#' @import htmltools
#' @import htmlwidgets
#' @export
%s <- function(..., width, height){
  params = list(...)
  createWidget(
    name = %s,
    params,
    package = %s
  )
}
# TODO: Add default width and height for widget
%sOutput <- function(outputId, width, height){
 shinyWidgetOutput(outputId, '%s', width, height, package = '%s')
}


render%s <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, %sOutput, env, quoted = TRUE)
}"

  capName = function(name){
    paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  }
  sprintf(tpl, name, name, package, name, name, package, capName(name), name)
}
