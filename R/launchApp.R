
#' launches the shinyAppDemo app
#'
#' @export twitter.data
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny
#'

twitter.data <- function() {
  shiny::shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
