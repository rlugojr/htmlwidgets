#' @export
add_new_widget <- function(name, package){
  if (!file.exists('DESCRIPTION')){
    stop("You need to create a package to house your widget first!", call. = F)
  }
  add_widget_constructor(name, package)
  add_widget_yaml(name)
  add_widget_js(name)
}

add_widget_constructor <- function(name, package = name){
  tpl <- "#' <Add Title>
#'
#' <Add Description>
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

#' Widget output function for use in Shiny App 
#' 
#' <TODO: Add default width and height for widget>
#' @export
%sOutput <- function(outputId, width, height){
 shinyWidgetOutput(outputId, '%s', width, height, package = '%s')
}

#' Widget render function for use in Shiny App
#' @export
render%s <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, %sOutput, env, quoted = TRUE)
}"

  capName = function(name){
    paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  }
  cat(
    sprintf(tpl, name, name, package, name, name, package, 
      capName(name), name),
    file = sprintf("R/%s.R", name)
  )
  message('Created boilerplate for widget constructor ', 
    sprintf("R/%s.R", name)
  )
}

add_widget_yaml <- function(name){
  tpl <- "# widget dependencies
dependencies:
  - name:
    version:
    src:
    script:
    stylesheet:
"
  if (!file.exists('inst/htmlwidgets')){
    dir.create('inst/htmlwidgets')
  }
  cat(tpl, file = sprintf('inst/htmlwidgets/%s.yaml', name))
  message('Created boilerplate for widget dependencies at ', 
    sprintf('inst/htmlwidgets/%s.yaml', name)
  )
}

add_widget_js <- function(name){
  tpl <- "// widget binding
HTMLWidgets.widget({
  name: '%s',
  type: 'output',
  initialize: function(el, width, height){

  },
  renderValue: function(el, data){

  }
})  
"
  cat(sprintf(tpl, name), file = sprintf('inst/htmlwidgets/%s.js', name))
  message('Created boilerplate for widget javascript bindings at', 
    sprintf('inst/htmlwidgets/%s.js', name)
  )
}

