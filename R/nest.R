##' Helper function to return a value for nest_dir
##' (empty strings for empty lists)
##' @title bname
##' @param ff Filename or empty list (result from nest_dir)
##' @return basename of file or an empty string
##' @export
bname <- function(ff) {
  if (length(ff)) {
    tryCatch({
      basename(ff)
    }, error = function(e) e)
  }
}

##' Make a nested list out of a directory
##' Note: there might be problems with document name length on windows.
##' tools::file_ext() was the quickest check for directory I could think of,
##' but there is probably something better (pattern "/$" or similar?)
##' @title nest_dir
##' @param path Base directory to start nesting from
##' @param value function to assign a value for each node (default `bname`)
##' @return nested list
##' @importFrom tools file_ext
##' @export
nest_dir <- function(path, value=bname) {
    isdir <- tools::file_ext(path) == ""
    if (!isdir) {
        value(path)
    } else {
        files <- list.files(path, full.names=TRUE, include.dirs = TRUE)
        if (!length(files)) {  # empty list will fail with shinyTree?
            value(files)
        } else {
            out <- lapply(files, nest_dir, value=value)
            tryCatch({
                names(out) <- basename(files)
            }, error=function(e) { print(e) })
            out
        }
    }
}

## Remove all NULLs from nested directories
##' @title remove_nulls
##' @param nest nested list
##' @return nested list with all NULLs removed (and all branches containing only NULLs)
##' @export
remove_nulls <- function(nest) {
  if (is.list(nest)) {
    inds <- vapply(nest, FUN=function(x) all(is.null(unlist(x, use.names=FALSE))),
                   FUN.VALUE = logical(1), USE.NAMES = FALSE)
    nest <- nest[!inds]  # remove branches that are all NULL
    lapply(nest, remove_nulls)
  } else nest
}

## Keep only specific files in nested directories
##' @title trim_nest
##' @param lst nested list
##' @param files values to keep in nested list
##' @return nested list only containing files (and branches with those files)
##' @export
trim_nest <- function(lst, files) {
  ## First replace everything not in files with NULL
  f <- function(x) if (x %in% files) x else NULL
  res <- rapply(lst, f, how="replace")  
  return( remove_nulls(res) )
}

