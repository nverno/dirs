##' Make an interactive shiny app to explore the directory
##' @importFrom sync.afs get_afs
##' @import shiny
##' @import shinyTree
explore_dir <- function(path = sync.afs::get_afs(), files=NULL) {
    if (!file.exists(path)) stop("Not connected to AFS or the path is wrong.")
    lst <- nest_dir(path)
    
    ui <- pageWithSidebar(
        headerPanel('Search the AFS directory'),
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
