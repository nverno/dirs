##' @title Make an interactive shiny app to explore the directory
##' @import shiny
##' @import shinyTree
##' @param path path to directory to visualize
##' @param files keep only these files in nested list (default to NULL)
explore_dir <- function(path, files=NULL) {
  if (!file.exists(path)) stop("path is not a valid directory.")
  lst <- nest_dir(path)
  if (!is.null(files)) lst <- trim_nest(lst, files)
  
  ui <- pageWithSidebar(
    headerPanel('Search the directory'),
    sidebarPanel(
      p(HTML('Search for a file'))
    ),
    mainPanel(
      shinyTree('tree', search=TRUE)
    )
  )

  server <- function(input, output, session) {
    output$tree <- renderTree({
      lst
    })
  }
  runApp(list(ui=ui, server=server))

}

