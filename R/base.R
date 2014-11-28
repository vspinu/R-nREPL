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
##' @import evaluate
##' @import bencode
NULL


### COUNTER

message_id <- 0L

uid <- function(){
    message_id <<- message_id + 1L
    message_id
}


### SESSIONS

sessions <- new.env()
.default_session_id <- "R/default"

create_session <- function(transport, from_session = NULL){
    if(is.null(from_session)){
        id <- .default_session_id
        if(is.null(sessions[[id]]))
            sessions[[id]] <- list(id = id)
        sessions[[id]]
    } else {
        id <- tempfile("", "R")
        sessions[[id]] <- assoc(from_session, id = id)
        sessions[[id]]
    }
}

pre_handle <- function(h){
    function(id, op, transport, session = NULL, ...){
        cat(as.character(Sys.time()), "-->>", "[", id, "]", op, "\n")
        if(is.null(session)){
            ## create a new session each time
            the_session <- create_session(transport)
            session <- the_session[["id"]]
        } else if ( is.null(the_session <- sessions[[session]]) ){
            ## we check here for session id for any mw!
            transport$write(list(id = id, session = session,
                                 status = c("error", "unknown-session", "done")))
        }    
        h(id = id, op = op, transport = transport, session = session, ...)
    }
}


### MIDDLEWARE NUTS AND BOLTS 

unknown_op <- function(op, transport, ...){
    resp <- respfor(list(...), op = op, status = c("error", "unknown-op", "done"))
    transport$write(resp)
}

linearize_mws <- function(mws){
    len <- length(mws)
    nms <- names(mws)
    reqexp <- matrix(0L, nrow = len, ncol = len, dimnames = list(nms, nms))
    for(nm in nms){
        desc <- attr(mws[[nm]], "descriptor")
        if(length(desc$expects))
            reqexp[desc$expects, nm] <- reqexp[desc$expects, nm] + 1L
        if(length(desc$requires))
            reqexp[desc$requires, nm] <- reqexp[desc$requires, nm] - 1L
    }
    ## print(reqexp)
    mws[do.call(order, as.data.frame(reqexp))]
}


### INTERNAL UTILS

assoc <- function(obj, ...){
    dots <- list(...)
    for(nm in names(dots))
        obj[[nm]] <- dots[[nm]]
    obj
}

dissoc <- function(obj, ..., keys = c()){
    keys <- c(unlist(), keys)
    obj[[keys]] <- NULL
    obj
}
