##' Default middleware.
##'
##' All default middlewares are stored within \code{middlewares} environment
##' inside this package. Users and add-on packages can append custom middleware
##' to this environment. See help page of \code{\link{middleware}} for more
##' details.
##'
##' Default middlewares are:
##' \itemize{
##'
##'   \item{\code{mw_session}: }{Return a handler for interactive evaluation. Provides
##' "eval" operation.}
##'      
##'   \item{\code{mw_describe}: }{Return a handler for session management. Supported ops
##' are "clone", "close" and "ls-sessions".}
##'
##'   \item{\code{mw_eval}: }{Return a handler for interactive evaluation. Provides
##' "eval" operation.}
##' 
##' }
##' @name middlewares
NULL



## DESCRIBE

.nrepl_version <- c(0L, 0L, 1L)

.nrepl_versions <- function(verbose = F){
    bendict(major = .nrepl_version[[1]],
            minor = .nrepl_version[[2]],
            incremental = .nrepl_version[[3]], 
            'version-string' = paste(.nrepl_version, collapse = "."))
}

.R_versions <- function(verbose = F){
    rv <-
        if(verbose) unclass(R.version)
        else list() 
    rv3 <- as.vector(unclass(getRversion()))[[1]]
    rv[c("major", "minor", "incremental")] <- rv3
    rv[["version-string"]] <- R.version.string
    rv[["version.string"]] <- NULL
    as.bendict(rv)
}

##' @rdname middlewares
##' @param handler Handler function. See \code{\link{middleware}}.
##' @export
mw_describe <-
    middleware("mw_describe", handles = "describe",
               fun =
                 function(h){
                     function(op, tr, ops, `verbose?` = FALSE, ...){
                         if( op == "describe"){
                             resp <- respfor(
                                 list(...),
                                 ops = if(`verbose?`) ops
                                       else sapply(ops, function(x) bendict(),
                                                   simplify=F),
                                 versions = bendict(
                                     nrepl = .nrepl_versions(`verbose?`), 
                                     R = .R_versions(`verbose?`)),
                                 status = list("done"))
                             tr$write(resp)
                         } else {
                             h(op = op, tr = tr, ...)
                         }
                     }
                 })



## SESSION

##' @rdname middlewares 
##' @export
mw_session <-
    middleware("mw_session", handles = c("close", "ls-sessions", "clone"),
               fun =
                 function(h){
                     function(op, tr, session = .default_session_id, ...){
                         the_session <- sessions[[session]]
                         msg <- assoc(list(...), session = session)
                         switch(op,
                                "clone" = {
                                    new_session <- create_session(tr, the_session)
                                    tr$write(respfor(msg,
                                                     'new-session' = new_session[["id"]], 
                                                     status = list("done", "session-cloned")))
                                },
                                "close" = {
                                    sessions[[the_session[["id"]]]] <- NULL
                                    tr$write(respfor(msg,
                                                     status = list("done", "session-closed")))
                                },
                                "ls-sessions" = {
                                    tr$write(respfor(msg,
                                                     status = list("done"),
                                                     sessions = ls(sessions)))
                                },
                                h(op = op, tr = tr, session = session, ...))
                     }
                 })



## EVAL

eval_handler <- function(msg, tr = transport_bencode){
    gr_id <- 0L
    new_output_handler(
        source = function(x) NULL,
        text = function(x){
            tr$write(respfor(msg, out = x, status = list("eval-out")), F)
        },
        value = function(x){
            tr$write(respfor(msg, value = x, status = list("eval-value")), F)
        }, 
        message = function(x){
            tr$write(respfor(msg, message = x$message, status = list("eval-message")), F)
        },
        warning = function(x){
            tr$write(respfor(msg, warning = x$message, status = list("eval-warning")), F)
        },
        error = function(x){
            tr$write(respfor(msg, error = x$message, status = list("eval-error")), F)
        },
        graphics = function(x){
            tr$write(respfor(msg, graphics = sprintf("[%d]", gr_id),
                             status = list("eval-graphics") ), F)
            gr_id <<- gr_id + 1L
        })
}

##' @rdname middlewares
##' @export
mw_eval <-
    middleware("mw_eval", handles = c("eval"),
               fun =
                 function(h){
                     function(op, tr, code = NULL, ...){
                         if(op == "eval"){
                             msg <- list(...)
                             if( is.null(code) )
                                 tr$write(
                                     respfor(msg, status = list("error", "no-code", "done")))
                             else {
                                 tryCatch(
                                     evaluate(code, new_device = F, stop_on_error = 1L,
                                              output_handler =
                                                eval_handler(msg, tr = tr)),
                                     error = function(x){
                                         tr$write(respfor(msg, error = x$message,
                                                          status = list("eval-error")), F)
                                     })
                                 tr$write(respfor(msg, status = list("done")))
                             }
                         } else
                             h(op = op, tr = tr, ...)
                     }
                 })


##' @export
middlewares <-
    list2env(list(session = mw_session, 
                  describe = mw_describe,
                  eval = mw_eval))


