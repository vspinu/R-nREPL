
##' Low level functions for client connections.
##'
##' Functions to perform the connect -> request -> receive -> close cycle.
##' 
##' In order to communicate to nREPL server you need first to create a
##' connection object with \code{connect}, then feed it to \code{client}
##' constructor which returns a function of variable arity.
##'
##' Receive responses from nREPL one at a time by calling the client function
##' with no arguments. Send messages to nREPL server by calling client function
##' with key-value named arguments representing a message.
##'
##' \code{sync_request} sends a requests and waits till the all the messages
##' associates with the current request have been received. Then returns a list
##' of the \code{bendict} responses. \code{sync_request0} is like
##' \code{sync_request} but combines all the responses from \code{sync_request}
##' into one \code{bendict} object.
##'
##' As R has no multi-thread support, asynchronous requests are not yet
##' implemented.
##' @name client 
##' @seealso \code{\link{transport}}
NULL

##' @rdname client
##' @param port Port number of a running nREPL instance .
##' @param host Host address. Defaults to "localhost".
##' @param transport_fn Constructor that returns a transport connection
##' object. See \code{\link{transport}}.
##' @export 
connect_nrepl <- function(port = 4005, host = "localhost",
                          transport_fn = transport_bencode, verbose = FALSE){
    con <- socketConnection(host = host, port = port, open = "r+b", blocking = T)
    structure(transport_fn(con, verbose = verbose), class = "nREPL")
}

##' @rdname client
##' @export
connect_epc <- function(port = 4006, host = "localhost",
                        transport_fn = transport_swank, verbose = FALSE){
    con <- socketConnection(host = host, port = port, open = "r+b", blocking = T)
    structure(transport_fn(con, verbose = verbose), class = "EPC")
}


##' @rdname client
##' @param transport Transport connection object returned by \code{connect} or a
##' transport constructor such as \code{\link{transport_bencode}}.
##' @export
client <- function(transport){
    UseMethod("client")
}

##' @export
client.nREPL <- function(transport){
    ## client function: no arg -> read; with args -> write
    transport <- transport
    function(..., timeout = NULL){
        mes <- list(...)
        if(length(mes) == 0L)
            transport$read(timeout)
        else {
            if(is.null(mes[["id"]]))
                mes[["id"]] <- uid()
            transport$write(mes)
        }
    }
}

##' @export
client.EPC <- function(transport){
    ## client function: no arg -> read; with args -> write
    structure(function(..., timeout = NULL){
        mes <- list(...)
        if(length(mes) == 0L)
            transport$read(timeout)
        else {
            transport$write(mes)
        }
    }, class = c(class(transport), "client"))
}

##' @rdname client
##' @param client Client function produced by \code{client} constructor.
##' @param op Name of the requested operation (for example, "eval", "describe",
##' "load-file" etc)
##' @param ... Key-value pairs of the request to be sent to nREPL server.
##' @export
sync_request <- function(client, op, ...){
    UseMethod("sync_request")
}

##' @export
sync_request.nREPL <- function(client, op, ...){
    mes <- list(...)
    if(is.null(mes[["id"]])) mes[["id"]] <- uid()
    mes[["op"]] <- op
    do.call(client, mes)
    id <- mes[["id"]]
    out <- client(timeout = 1)
    accum <- list()
    while( is.null(out) || out[["id"]] != id ||
           !any(c("error", "done") %in% out[["status"]]) ){
               if(!is.null(out) && out[["id"]] == id)
                   accum[[length(accum) + 1L]] <- out
               out <- client(timeout = 1)
           }
    accum[[length(accum) + 1L]] <- out
    accum
}

##' @export
sync_request.EPC <- function(client, type, ...){
    id <- uid()
    mes <- c(list(type, id), list(...))
    do.call(client, mes)
    out <- client(timeout = 1)
    accum <- list()
    while(is.null(out) || !as.character(out[[1]]) %in% c("return", "return-error", "epc-error")){
               if(!is.null(out) && out[["id"]] == id)
                  accum[[length(accum) + 1L]] <- out
              out <- client(timeout = 1)
          }
    accum[[length(accum) + 1L]] <- out
    accum
}


##' @rdname client
##' @export
sync_request0 <- function(client, op, ...){
    UseMethod("sync_request0")
}

##' @export
sync_request0.EPC <- function(client, type, ...){
    out <- sync_request(client = client, type = type, ...)
    id <- out[[1]]$id
    nms <- as.character(unlist(lapply(out, "[[", "op")))
    args <- sapply(out, "[[", "args", simplify = F)
    c(list(id = id),
      structure(args, names = nms))
}

##' @export
sync_request0.nREPL <- function(client, op, ...)
    combine_responses.nREPL(sync_request(client = client, op = op, ...))

combine_responses.nREPL <- function(response_list){
    ## Certain message slots are combined in special ways:
    ## - only the last :id, :ns and :session is retained
    ## - :value and :status  are accumulated into a vector
    ## - string values (associated with e.g. :out and :err) are concatenated
    Reduce(function(accum, el){
        for(nm in names(el)){
            accum[[nm]] <-
                switch(nm, 
                       id = ,
                       session =, 
                       ns = el[[nm]], 
                       status =,
                       value = c(accum[[nm]], el[[nm]]),
                       if(is.character(el[[nm]]))
                           paste0(accum[[nm]], el[[nm]])
                       else
                           c(accum[[nm]], el[[nm]]))
        }
        accum
    }, response_list, list())
}

