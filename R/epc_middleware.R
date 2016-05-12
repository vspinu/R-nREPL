##' @include middleware.R
NULL

##' @export
epc_msg <- function(..., .lst = list()){
    structure(c(list(...), .lst), class = c("EPC", "rpc-message"))
}

##' EPC middleware
##'
##' @name epc_middleware
##' @export
resp_for.EPC <- function(msg, ..., op = q(return), .lst = list()){
    stopifnot(!is.null(msg[["id"]]))
    c(list(op, msg[["id"]], ...),  .lst)
}

##' @rdname epc_middleware
##' @export
error_for.EPC <- function(msg, message = "", ...) {
    resp_for.EPC(msg,
                 op = q("epc-error"), 
                 do.call(sprintf, c(list(message), list(...))))
}


##' @rdname epc_middleware
##' @param handler Handler function. See \code{\link{middleware}}.
##' @export
mw_epc_describe <-
    middleware("describe",
               handles = list(methods = list(doc = "List all available methods",
                                             returns = "List of (name, args, description)"),
                              describe = list(doc = "Full description of the environment", 
                                              returns = "nREPL style dict of version and middlware descriptions")),
               function(h) {
                   function(tr, msg) {
                       switch(msg$op,
                              describe = {
                                  resp <- resp_for(msg, ops = msg$OPS, 
                                                   versions = list(R = .R_versions(verbose)))
                                  tr$write(resp)
                              }, 
                              methods = {
                                  OPS <- msg$OPS
                                  out <- lapply(names(OPS),
                                                function(nm)
                                                    list(nm, OPS[[nm]]$requires, OPS[[nm]]$doc))
                                  tr$write(resp_for(msg, out))
                              },
                              h(tr, msg))
                   }
               })

##' @rdname epc_middleware
##' @export
mw_epc_session <-
    middleware("session",
               handles = list(close = list(doc = "Quit connection."),
                              quit = list(doc = "Quit connection.")),
               function(h){
                   function(tr, msg){
                       switch(msg$op,
                              quit = , 
                              close = {
                                  tr$write(resp_for(msg, status = list("done", "session-closed")))
                                  condition <- simpleError("Connection ended by the client.")
                                  class(condition) <- c("quit", class(condition))
                                  signalCondition(condition)
                              },
                              h(tr, msg))
                   }
               })

##' @rdname epc_middleware
##' @export
mw_epc_echo <-
    middleware("echo",
               handles = list(echo = list(doc = "Echo the message back.")),
               function(h){
                   function(tr, msg){
                       if(msg$op == "echo") tr$write(resp_for(msg, .lst = msg$args))
                       else h(tr, msg)
                   }
               })

##' @export
epc_middlewares <- 
    list(echo = mw_epc_echo, 
         session = mw_epc_session, 
         describe = mw_epc_describe)


