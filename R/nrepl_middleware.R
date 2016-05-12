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
##' @name nrepl_middleware
##' @include middleware.R
NULL


##' @rdname nrepl_middleware
##' @export
resp_for.nREPL <- function(msg, ..., lst = list()){
    stopifnot(!is.null(msg[["id"]]))
    if (is.null(msg[["session"]])) msg[["session"]] <- "R/unknown"
    resp <- c(list(...), lst)
    resp[c("id", "session")] <- msg[c("id", "session")]
    resp
}

##' @rdname nrepl_middleware
##' @export
error_for.nREPL <- function(msg, message = "", ..., additional_status = list()){
    resp_for(msg,
             error = do.call(sprintf, c(list(message), list(...))),
             status = c(list("error", "done"), additional_status))
}

nrepl_create_session <- function(from_session = NULL){
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

nrepl_pre_handle <- function(h){
    function(tr, msg){
        cat(as.character(Sys.time()), sprintf("-->> [%s]", mgs$id), msg$op, "\n")
        if(is.null(msg$session)){
            ## create a new session each time
            the_session <- nrepl_create_session()
            session <- the_session[["id"]]
        } else if ( is.null(the_session <- sessions[[session]]) ){
            ## we check here for session id for any mw!
            tr$write(list(id = msg$id, session = msg$session,
                          status = c("error", "unknown-session", "done")))
            return()
        }
        h(tr, msg)
    }
}

.nrepl_version <- c(0L, 0L, 1L)

.nrepl_versions <- function(verbose = F){
    bendict(major = .nrepl_version[[1]],
            minor = .nrepl_version[[2]],
            incremental = .nrepl_version[[3]], 
            'version-string' = paste(.nrepl_version, collapse = "."))
}

##' @rdname nrepl_middleware
##' @param handler Handler function. See \code{\link{middleware}}.
##' @export
mw_describe <-
    middleware("mw_describe", handles = "describe",
               fun =
                   function(h){
                       function(tr, msg){
                           verbose <- dget(msg, "verbose?", FALSE)
                           if(msg$op == "describe"){
                               resp <- resp_for(msg, 
                                                ops = if(verbose) msg$OPS
                                                      else sapply(msg$OPS, function(x) bendict(), simplify=F),
                                                versions = bendict(
                                                    nrepl = .nrepl_versions(verbose), 
                                                    R = .R_versions(verbose)),
                                                status = list("done"))
                               tr$write(resp)
                           } else {
                               h(tr, msg)
                           }
                       }
                   })

##' @rdname nrepl_middleware 
##' @export
mw_session <-
    middleware("mw_session", handles = list(close = NULL, "ls-sessions" = NULL, clone = NULL),
               function(h){
                   function(tr, msg){
                       ses <- sessions[[dget(msg, "session", .default_session_id)]]
                       msg <- assoc(msg, session = ses)
                       switch(op,
                              "clone" = {
                                  new_session <- nrepl_create_session(the_session)
                                  tr$write(resp_for(msg,
                                                    'new-session' = new_session[["id"]], 
                                                    status = list("done", "session-cloned")))
                              },
                              "close" = {
                                  sessions[[ses[["id"]]]] <- NULL
                                  tr$write(resp_for(msg, status = list("done", "session-closed")))
                              },
                              "ls-sessions" = {
                                  tr$write(resp_for(msg, status = list("done"),
                                                    sessions = ls(sessions)))
                              },
                              h(tr, msg))
                   }
               })


eval_handler <- function(tr, msg){
    gr_id <- 0L
    new_output_handler(
        source = function(x) NULL,
        text = function(x){
            tr$write(resp_for(msg, out = x, status = list("eval-out")), F)
        },
        value = function(x){
            tr$write(resp_for(msg, value = x, status = list("eval-value")), F)
        }, 
        message = function(x){
            tr$write(resp_for(msg, message = x$message, status = list("eval-message")), F)
        },
        warning = function(x){
            tr$write(resp_for(msg, warning = x$message, status = list("eval-warning")), F)
        },
        error = function(x){
            tr$write(resp_for(msg, error = x$message, status = list("eval-error")), F)
        },
        graphics = function(x){
            tr$write(resp_for(msg, graphics = sprintf("[%d]", gr_id),
                             status = list("eval-graphics") ), F)
            gr_id <<- gr_id + 1L
        })
}

##' @rdname nrepl_middleware
##' @export
mw_eval <-
    middleware("mw_eval", handles = list(eval = NULL),
               fun =
                 function(h){
                     function(tr, msg){
                         if(msg$op == "eval"){
                             code <- msg$code
                             if(is.null(code))
                                 tr$write(resp_for(msg, status = list("error", "no-code", "done")))
                             else {
                                 tryCatch(
                                     evaluate(code, new_device = F, stop_on_error = 1L,
                                              output_handler = eval_handler(msg, tr = tr)),
                                     error = function(x){
                                         tr$write(resp_for(msg, error = x$message,
                                                           status = list("eval-error")), F)
                                     })
                                 tr$write(resp_for(msg, status = list("done")))
                             }
                         } else
                             h(tr, msg)
                     }
                 })



##' @export
nrepl_middlewares <-
    list(session = mw_session, 
         describe = mw_describe,
         eval = mw_eval)


