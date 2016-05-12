
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
##' \code{\link{nrepl_resp_for}} utility function.
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
##' \code{\link{nrepl_resp_for}}, \code{\link{mw_describe}}, \code{\link{mw_describe}},
##' \code{\link{mw_eval}}, \code{\link{mw_session}}.
##' @return A wrapped middleware function \code{fun} with a \code{descriptor}
##' attribute for internal use.
##' @export
middleware <- function(name,
                       handles, requires = c(), expects = c(), 
                       fun) {

    if (is.function(requires)){
        fun <- requires
        requires <- list()
    }


    if (is.function(expects)){
        fun <- expects
        expects <- list()
    }

    descriptor <-
        list(handles = handles,
             expects = union(expects, "describe"),
             requires = requires)
    
    wrapped_fun <-
        function(handler){
            h2 <- fun(handler)
            function(tr, msg){
                if( msg$op == "describe" || msg$op == "methods"){
                    msg$OPS <- c(msg$OPS, handles)
                    h2(tr, msg)
                } else {
                    h2(tr, msg)
                }
            }
        }
    attr(wrapped_fun, "descriptor") <- descriptor

    wrapped_fun
}

##' @rdname middleware_utils
##' @param message error message to be sent through
##' @export
error_for <- function(msg, message = "", ..., additional_status = list()){
    UseMethod("error_for")
}

##' @rdname middleware_utils
##' @export
resp_for <- function(msg, ..., .lst = list()){
    UseMethod("resp_for")
}

##' @rdname middleware_utils
##' @export
unknown_op <- function(tr, msg){
    resp <- error_for(msg, "Unknown op: '%s'",  msg$op)
    tr$write(resp)
}

##' @rdname middleware_utils
##' @export
backToTop <- function(){
    cond <- simpleError("Return to top level")
    class(cond) <- c("backToTop", class(cond))
    signalCondition(cond)
}

##' @rdname middleware_utils
##' @param msg Error message to send back to client on backToTop condition
##' @export
backToTopError <- function(message, ..., status = list()){
    cond <- list(message = do.call(sprintf, c(list(message), list(...))),
                 status = status)
    class(cond) <- c("backToTopError", "error", "condition")
    stop(cond)
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

##' @rdname middleware_utils
##' @param mw Middleware for testing
##' @export
test_middleware <- function(mw, ...){
    con <- textConnection("test", open = "w")
    on.exit(close(con))
    h <- pre_handle(mw(unknown_op))
    trs <- nREPL:::transport_print(con)
    id <- 9999L
    tryCatch(h(id = id, tr = trs, ...),
             backToTopError = function(c){
                 trs$write(error_for(c(list(id = id, session = "R/test"), list(...)),
                                     c$message, additional_status = c$status))
             }, 
             backToTop = function(c) NULL)
    cat(textConnectionValue(con), sep = "\n")
}
