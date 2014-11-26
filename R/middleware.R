##' Create middleware functions.
##'
##' Middleware is a higher-order function of one argument that accepts a handler
##' and returns a new handler in order to compose additional functionality onto
##' or around the original handler. By convention middleware functions are
##' prefixed with `mw_`.
##'
##' Handler is a function that accepts incoming message from the client. Each
##' handler must accept at least two named arguments \code{op}, naming and
##' operation and \code{\link{transport}} - a structure that provides the
##' phisical transportation layer. By inspecting \code{op} argument the handler
##' must decide whether to handle this message or to pass it through to
##' downstream handlers. Thus handler's argument list must contain \code{...}
##' argument. Handler's return values are ignores and each handler must send a
##' response dictionary to the client with \code{transport$write(response)}
##' idem, where \code{response} is a named list of response arguments. Each
##' \code{response} must contain at least two arguments \code{id} of the
##' incoming message and \code{session}. This is best done with a helper
##' \code{\link{respfor}} utility function.
##'
##' For a detailed technical details on the protocol see
##' \url{https://github.com/clojure/tools.nrepl} and the package code.
##'
##' In nREPL package, middlewares are stored in \code{middlewares} environment
##' inside. An addon package can populate that environment with additional
##' middlewares. Alternatively \code{\link{start_server}} function accepts
##' \code{additional_middlewares} arguments.
##' 
##' @param name String giving the names of the middleware. Commonly, is the name
##' of the 
##' @param handles Character verctor of operations that thhis middleware
##' handles.
##' @param requires Character vector of operations that must exist in the final
##' stack at a higher level than the current middleware's handler.
##' @param expects Character vector of operations that must exist in the final
##' stack at a lower level than the current middleware's handler. This is useful
##' when the middleware pre-process the message in order to feed it to some
##' other handler.
##' @param fun Actual middleware function that accepts one argument, a handler,
##' and returns a handler.
##' @seealso \code{\link{start_server}}, \code{\link{transport}},
##' \code{\link{respfor}}, \code{\link{mw_describe}}, \code{\link{mw_describe}},
##' \code{\link{mw_eval}}, \code{\link{mw_session}}.
##' @return A wrapped middleware function \code{fun} with a \code{descriptor}
##' attribute for internal use.
##' @export
middleware <- function(name,
                       handles = character(), 
                       requires = character(),
                       expects = character(),
                       fun){
    descriptor <-
        list(handles = handles,
             expects = union(expects, "describe"),
             requires = requires)
    
    wrapped_fun <-
        function(handler){
            h2 <- fun(handler)
            function(op, ops = list(), ...){
                if( op == "describe" ){
                    ops[[name]] <- handles
                    h2(op = op, ops = ops, ...)
                } else {
                    h2(op = op, ...)
                }
            }
        }
    attr(wrapped_fun, "descriptor") <- descriptor

    wrapped_fun
}

##' @rdname middleware
##' @param msg Incoming message containing \code{id} and \code{session}
##' elements.
##' @param ... key-value pairs of response
##' @param lst A list which is merged with the elements in \code{...}
##' @export
respfor <- function(msg, ..., lst = list()){
    stopifnot(!is.null(msg[["id"]]))
    stopifnot(!is.null(msg[["session"]]))
    resp <- c(list(...), lst)
    resp[c("id", "session")] <- msg[c("id", "session")]
    resp
}




## DESCIBE

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
                     function(op, transport, ops, `verbose?` = FALSE, ...){
                         if( op == "describe"){
                             resp <- respfor(
                                 list(...),
                                 ops = if(`verbose?`) ops
                                       else sapply(ops, function(x) bendict(),
                                                   simplify=F),
                                 versions = bendict(
                                     nrepl = .nrepl_versions(`verbose?`), 
                                     R = .R_versions(`verbose?`)),
                                 status = "done")
                             transport$write(resp)
                         } else {
                             h(op = op, transport = transport, ...)
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
                     function(op, transport, session = .default_session_id, ...){
                         the_session <- sessions[[session]]
                         msg <- assoc(list(...), session = session)
                         switch(op,
                                "clone" = {
                                    new_session <- create_session(transport, the_session)
                                    transport$write(respfor(msg,
                                                            'new-session' = new_session[["id"]], 
                                                            status = c("done", "session-cloned")))
                                },
                                "close" = {
                                    sessions[[the_session[["id"]]]] <- NULL
                                    transport$write(respfor(msg,
                                                            status = c("done", "session-closed")))
                                },
                                "ls-sessions" = {
                                    transport$write(respfor(msg,
                                                            status = "done",
                                                            sessions = ls(sessions)))
                                },
                                h(op = op, transport = transport, session = session, ...))
                     }
                 })



## EVAL

eval_handler <- function(msg, transport = transport_bencode){
    gr_id <- 0L
    new_output_handler(
        source = function(x) NULL,
        text = function(x){
            transport$write(respfor(msg, out = x, status = "eval-out"))
        },
        value = function(x){
            transport$write(respfor(msg, value = x, status = "eval-value"))
        }, 
        message = function(x){
            transport$write(respfor(msg, message = x$message, status = "eval-message"))
        },
        warning = function(x){
            transport$write(respfor(msg, warning = x$message, status = "eval-warning"))
        },
        error = function(x){
            transport$write(respfor(msg, error = x$message, status = "eval-error"))
        },
        graphics = function(x){
            transport$write(respfor(msg, graphics = sprintf("[%d]", gr_id),
                                    status = "eval-graphics" ))
            gr_id <<- gr_id + 1L
        })
}

##' @rdname middlewares
##' @export
mw_eval <-
    middleware("mw_eval", handles = c("eval"),
               fun =
                 function(h){
                     function(op, transport, code = NULL, ...){
                         if(op == "eval"){
                             msg <- list(...)
                             if( is.null(code) )
                                 transport$write(
                                     respfor(msg, status = c("error", "no-code", "done")))
                             else {
                                 evaluate(code, new_device = F, stop_on_error = 1L,
                                          output_handler =
                                            eval_handler(msg, transport = transport))
                                 transport$write(respfor(msg, status = "done"))
                             }
                         } else
                             h(op = op, transport = transport, ...)
                     }
                 })



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

##' @export
middlewares <-
    list2env(list(session = mw_session, 
                  describe = mw_describe,
                  eval = mw_eval))


