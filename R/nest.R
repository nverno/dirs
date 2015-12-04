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
        }, error = function(e) "")
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

clean_list <- function(lst, fun = is.null, recursive = FALSE) {
  if(recursive) {
    res <- lapply(lst, function(x) {
      if (is.list(x)) clean_list(x, fun, TRUE)
      else x
    })
  }
  lst[!vapply(lst, fun, logical(1L))]
  lst
}

trim_nest <- function(lst, files) {
    f <- function(x) if (x %in% files) x else NULL
    res <- rapply(lst, f, how="replace")
    res <- res[vapply(res, FUN=function(i) any(!is.null(unlist(i, use.names=FALSE))),
                     FUN.VALUE = logical(1))]
    
    cond <- function(x) {
        (is.null(x) |
         !length(x) |
         all(unlist(lapply(x, function(i) is.null(i) | !length(i)), use.names=FALSE)))
    }

    rmNull <- function(x) {
        x <- Filter(Negate(cond), x)
        lapply(x, function(y) if (is.list(y)) rmNull(y) else y)
    }
    
    return( rmNull(res) )
}

