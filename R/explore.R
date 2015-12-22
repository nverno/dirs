##' Interactive interface to AFS files (requires AFS connection -- not tested on MAC).
##' @title Make an interactive shiny app to explore the directory
##' @param path path to directory to visualize
##' @param files keep only these files in nested list (default to NULL)
##' @examples
##' \dontrun{
##'   ## Interactive visualization of tracked files on AFS
##'   explore_dir(get_afs(), files=data_key$filename)
##' }
##' @import shiny
##' @import shinyTree
##' @export
explore_dir <- function(path, files=NULL) {
  require(shinyTree)
  if (!file.exists(path)) stop("path is not a valid directory.")
  lst <- nest_dir(path)
  if (!is.null(files)) lst <- trim_nest(lst, files)
  
  ui <- shiny::pageWithSidebar(
    shiny::headerPanel('Search the directory'),
    shiny::sidebarPanel(
      shiny::p(HTML('Search for a file'))
    ),
    shiny::mainPanel(
      shinyTree::shinyTree('tree', search=TRUE)
    )
  )

  server <- function(input, output, session) {
    output$tree <- shinyTree::renderTree(lst)
  }
  shiny::runApp(list(ui=ui, server=server))
}

