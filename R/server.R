##' nREPL Server.
##'
##' nREPL server is a blocking connection that waits for requests from the nREPL
##' client and sends the responses back.
##'
##' \code{default_handler} returns a handler which is a stack of handlers
##' produced by default middlewares.
##'
##' @name server
##' @param port Port number on which to start an nREPL server.
##' @param handler Function of variable arity to process incoming requests.
##' @param transport_fn Constructor that returns a transport connection
##' object. See \code{\link{transport}}.
##' @seealso \link{middlewares}
##' @export
start_nrepl <- function(port = 4005, ## as.integer(runif(1, 4000, 99999)), 
                        handler = nrepl_handler(),
                        transport_fn = transport_bencode){
    start_server(port, handler, transport_fn)
}


##' @export
##' @rdname server
start_epc <- function(port = 4005, ## as.integer(runif(1, 4000, 99999)), 
                      handler = epc_handler(),
                      transport_fn = transport_swank){
    start_server(port, handler, transport_fn)
}

##' @rdname server
##' @param additional_middlewares A list of middleware functions to merge into
##' the list of default \code{\link{middlewares}}
##' @export
nrepl_handler <- function(additional_middlewares = list()){
    mws <- c(as.list(nrepl_middlewares), additional_middlewares)
    mws <- linearize_mws(mws)
    nrepl_pre_handle(Reduce(function(f, h){ force(h); f(h)},
                            mws, init = unknown_op, right = T))
}

##' @rdname server
##' @export
epc_handler <- function(additional_middlewares = list()){
    mws <- c(as.list(epc_middlewares), additional_middlewares)
    mws <- linearize_mws(mws)
    Reduce(function(f, h){ force(h); f(h)},
           mws, init = unknown_op, right = T)
}

start_server <- function(port, handler, transport_fn, verbose = TRUE){
    run <- TRUE
    while(run){
        cat("nREPL server started on port", port, "\nWaiting for connection ... ")
        ss <- socketConnection(port = port, server = TRUE,
                               open = "r+b", blocking = TRUE)
        transport <- transport_fn(ss, verbose = verbose)
        on.exit(transport$close())
        cat("connected.\n")
        tryCatch(handle_messages(transport, handler),
                 endOfInput = function(c){
                     cat(c$message, "\n")
                     ## R doesn't allow re-connection, so close and restart
                     transport$close()
                 },
                 quit = function(c){
                     cat("Quiting ...\n")
                     transport$close()
                     run <<- FALSE
                 })
    }
}

handle_messages <- function(tr, handler){
    while(TRUE){
        ## ## read message (re-init every 10 sec)
        msg <- tr$read(10)
        
        ## tryCatch(msg <- tr$read(10),
        ##          ## error = function(e){
        ##          ##     cond <- simpleCondition(sprintf("%s\n", e$message))
        ##          ##     class(cond) <- c("endOfInput", class(cond))
        ##          ##     signalCondition(cond)
        ##          ## }, 
        ##          endOfInput = function(e) {
        ##              ## this is how we detect when connection was closed by peer
        ##              cond <- simpleCondition(sprintf("%s Client closed?\n", e$message))
        ##              class(cond) <- c("endOfInput", class(cond))
        ##              signalCondition(cond)
        ##          })

        ## handle
        if(!is.null(msg)){
            tryCatch(handler(tr, msg),
                     error = function(e){
                         cat("Unhandled exception on message\n")
                         print(msg)
                         cat(as.character(e))
                         tr$write(error_for(msg, e$message,
                                            additional_status = list("unhandled-exception")))
                     },
                     backToTopError = function(c){
                         tr$write(error_for(msg, c$message, additional_status = c$status))
                     }, 
                     backToTop = function(c) NULL)
        }
    }
}
