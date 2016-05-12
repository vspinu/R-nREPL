##' nREPL client and server for R 
##'
##' nREPL is *n*etwork REPL that provides a REPL server and client, along with
##' some common APIs of use to IDEs and other tools that may need to evaluate R
##' code in remote environments.
##'
##' nREPL protocol was designed by Chas Emerick to be used in Clojure REPLs and
##' IDEs, but it's not limited or tied to to Closure in any way.
##'
##' @docType package
##' @name nREPL-package
##' @aliases nREPL
##' @author Vitalie Spinu
##' @seealso \code{\link{start_server}}, \code{\link{client}},
##' \code{\link{middleware}}
##' @references Full documentation and demos: \url{http://yihui.name/knitr/};
##' FAQ's: \url{https://github.com/yihui/knitr/blob/master/FAQ.md}
NULL

sessions <- new.env()
.default_session_id <- "R/default"

##' Various utils
##'
##' @rdname middleware_utils
##' @export
q <- function(x){
    x <- as.character(substitute(x))
    attr(x, "quoted") <- TRUE
    x
}
uid <- local({
    message_id <- 0L
    function(){
        message_id <<- message_id + 1L
        message_id
    }
})

.R_versions <- function(verbose = F){
    rv <-
        if(verbose) unclass(R.version)
        else list() 
    rv3 <- as.vector(unclass(getRversion()))[[1]]
    rv[c("major", "minor", "incremental")] <- rv3
    rv[["version-string"]] <- R.version.string
    rv[["version.string"]] <- NULL
    rv
}

assoc <- function(obj, ..., non_null = FALSE){
    dots <- list(...)
    for(nm in names(dots)){
        if ( !(is.null(el <- dots[[nm]]) && non_null) )
            obj[[nm]] <- el
    }
    obj
}

dissoc <- function(obj, ..., keys = c()){
    keys <- c(unlist(), keys)
    obj[[keys]] <- NULL
    obj
}

dget <- function(obj, val, default = NULL){
    res <- obj[["val"]]
    if(is.null(res)) default
    else res
}
