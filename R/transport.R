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
##' @name transport
NULL

##' @param con Connection object as returned by \code{\link{socketConnection}}.
##' @rdname transport
##' @export
transport_bencode <- function(con, verbose = TRUE){
    stdin <- stdin()
    list(read =
           function(timeout = NULL){
               ## socketSelect accepts only integer seconds
               received <- 
                   if(!is.null(timeout))
                       socketSelect(list(con), timeout = ceiling(timeout))
                   else TRUE
               if(received) bdecode(con)
               else NULL
           }, 
         write =
           function(obj){
               ## if(verbose)
               ##     cat(as.character(Sys.time()),
               ##         "<<--", "[", obj[["id"]], "]", as.character(obj[["status"]]), "\n",
               ##         file = stdin)
               obj <- bencode(as.bendict(obj))
               writeChar(obj, con, eos = NULL)
               flush(con)
           }, 
         close =
           function(){
               close(con)
           },
         con = con)
}

## test only
transport_print <- function(con){
    list(read =
           function(timeout = NULL){
               stop("cannot read in test transport")
           }, 
         write =
           function(obj){
               ## obj <- bencode(as.bendict(obj))
               out <- capture.output(print(as.bendict(obj)))
               out <- paste(out, collapse = "\n")
               writeLines(out, con)
           }, 
         close =
           function(){},
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
