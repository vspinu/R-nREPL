##' Default transports.
##'
##' Transports are functions that return a list object which is a wraper arround
##' open \code{\link{socketConnection}}. Each transport function accept one
##' argument, a connection, and returns a list of at least 3 functions:
##' \code{write} - a function of one argument (a list request) that encodes and
##' then send the message to the nREPL server; \code{read} - function of one
##' argument (timeout) that waits for the incomming responses and decodes one
##' response at a time; \code{close} - function of no argment that closees the
##' connection.
##' @param con Connection object as returned by \code{\link{socketConnection}}.
##' @name transport
NULL

##' @rdname transport
##' @export
transport_bencode <- function(con, verbose = TRUE){
    loadNamespace("bencode")
    vbs <- verbose
    list(read =
             function(timeout = NULL, verbose = vbs){
                 ## socketSelect accepts only integer seconds
                 received <- 
                     if(!is.null(timeout))
                         socketSelect(list(con), timeout = ceiling(timeout))
                     else TRUE
                 if(received)
                     structure(bencode::bdecode(con), class = c("nREPL", "rpc-message"))
                 else NULL
             }, 
         write =
             function(obj, verbose = vbs){
                 if(verbose)
                     cat(as.character(Sys.time()),
                         sprintf("<<-- [%s]", obj[["id"]]), as.character(obj[["status"]]), "\n")
                 obj <- bencode(bencode::as.bendict(obj))
                 writeChar(obj, con, eos = NULL)
                 flush(con)
             }, 
         close =
             function(){
                 close(con)
             },
         con = con)
}

##' @rdname transport
##' @export
transport_swank <- function(con, verbose = TRUE){
    loadNamespace("elparser")
    vbs <- verbose
    list(read =
             function(timeout = NULL, verbose = vbs){
                 ## socketSelect accepts only integer seconds
                 received <- 
                     if(!is.null(timeout))
                         socketSelect(list(con), timeout = ceiling(timeout))
                     else TRUE

                 if (received){
                     xlen <- readChar(con, 6L, useBytes = T)
                     cat("received header length:", xlen, "\n")
                     
                     if (length(xlen) == 0L) {
                         condition <- simpleError("End of input. Client closed?")
                         class(condition) <- c("endOfInput", class(condition))
                         signalCondition(condition)
                         NULL
                     } else {

                         if (nchar(xlen) < 6){
                             ## fixme: need to loose here instead dying
                             stop(sprintf("Header of less than 6 bytes: '%s'", xlen))
                         }
                         len <- as.integer(as.hexmode(xlen))
                         ## waf? why epc sends trailing \n?
                         payload <- readChar(con, as.integer(len), useBytes = FALSE)
                         ## readChar(con, 1, useBytes = T)
                         if(verbose){
                             cat("<<---", paste0(xlen, payload) , "\n")
                         }
                         mess <- elparser::decode(payload)
                         op <- mess[[1]]
                         id <- mess[[2]]
                         if(as.character(op) == "call"){
                             op <- mess[[3]]
                             args <- mess[-(1:3)]
                         } else {
                             op <- mess[[1]]
                             args <- mess[-(1:2)]
                         }
                         epc_msg(op = op,
                                 id = mess[[2]],
                                 args = args)
                     }
                 } else NULL
             }, 
         write =
             function(obj, verbose = vbs){
                 obj <- elparser::encode(obj)
                 len <- sprintf("%06X", as.hexmode(nchar(obj) + 1L))
                 ## why "\n" in emacs-epc?
                 payload <- paste0(len, obj, "\n")
                 if(verbose)
                     ## cat(as.character(Sys.time()), sprintf("<<-- [%s]", paste(obj, collapse = " ")), "\n")
                     cat("--->>", payload, "\n")
                 ## writeChar(len, con, nchars = 6, eos = NULL, useBytes = T)
                 ## writeChar(obj, con, eos = "")
                 writeChar(payload, con, eos = NULL)
                 ## writeChar("000012(call 23 echo 34)", con, eos = "\n")
                 flush(con)
             }, 
         close =
             function(){
                 close(con)
             },
         con = con)
}

## test only
transport_print <- function(con = stdout(), verbose = TRUE){
    list(read =
             function(timeout = NULL, ...){
                 stop("cannot read in test transport")
             }, 
         write =
             function(obj, ...){
                 ## obj <- bencode(as.bendict(obj))
                 out <- capture.output(str(obj))
                 out <- paste(out, collapse = "\n")
                 writeLines(out, con)
             }, 
         close =
             function(){},
         con = con)
}

transport_return <- function(con = NULL, verbose = TRUE){
    list(read =
             function(timeout = NULL, ...){
                 stop("cannot read in test transport")
             }, 
         write = identity, 
         close = identity, 
         con = con)
}

## ## DO NOT DELETE (c level bdecoder. Stuck because connections are not available at C level)
## cache <- list()
## N <- 0L
## read_fn_str <- function(timeout = NULL){
##     ## socketSelect accepts only integer seconds
##     if(!is.null(timeout))
##         timeout <- ceiling(timeout)
##     on.exit({
##         cache <<- list()
##         N <<- 0L
##     })
##     mes <- readChar(conn, 1000, TRUE)
##     if( length(mes) == 0 ){
##         if(is.null(timeout)){
##             list()
##         } else {
##             socketSelect(list(conn), timeout = timeout)
##             read_fn(timeout = NULL)
##         }
##     } else {
##         N <<- N + 1L
##         cache[[N]] <<- mes
##         while("e" != substring(mes, first = nchar(mes))){
##             mes <- readChar(conn, 1000, TRUE)
##             N <<- N + 1L
##             cache[[N]] <<- mes
##         }
##         out <-
##             tryCatch(bdecode(do.call(paste0, c(cache, collapse = "\n"))),
##                      error = function(e){
##                          ## All errors are assumed to be incomplete
##                          ## message. Block and wait for new input.
##                          print(e)
##                          print(cache)
##                          if(isOpen(conn)){
##                              if(!is.null(timeout)){
##                                  socketSelect(list(conn), timeout = timeout)
##                              }
##                              read_fn(timeout = NULL)
##                          } else {
##                              signalCondition(e)
##                          } 
##                     })
##         if(is.bendict(out)) out <- list(out)
##         out
##     }
## }
