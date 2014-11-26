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
        function(h1){
            h2 <- fun(h1)
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

##' @describeIn middleware Response constructor utility.
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

