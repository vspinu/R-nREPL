onHeaders <- function(req){
    cat("[onHeaders] ", handle, "\n")
    print(ls.str(req))
    NULL
}
onBodyData <- function(req, bytes){
    cat("[onBodyData:]", as.character(bytes), "\n")
    print(ls.str(req))
    NULL
}
call <- function(req){
    cat("[call]\n")
    print(ls.str(req))
    NULL
}
onOpen <- function(handle, req){
    cat("[opened] ", handle, "\n")
    print(ls.str(req))
    NULL
}
onMessage <- function(...){
    cat("[onWSMessage]")
    NULL
}
onClose <- function(...){
    cat("[Closing]")
    NULL
}
## app <- list(call = call,
##             onWSOpen = onWSOpen,
##             onBodyData = onBodyData,
##             onWSMessage = onWSMessage)

tcp = function(host = '127.0.0.1', port = 5555) {
    server <-
        httpuv:::makeTcpServer(host, port,
                               onHeaders, 
                               onBodyData,
                               call,
                               onOpen,
                               onMessage,
                               onClose)
    on.exit(stopServer(server))
    assign("stopped", FALSE, httpuv:::.globals)
    while (!httpuv:::.globals$stopped) {
        service(100)
        Sys.sleep(0.001)
    }
}

tcp()

con <- socketConnection(port = 5555, blocking = TRUE)
writeLines("bla bla bla\n", con)
gsub(" *$", "", readLines(con))
close(con)


httd(daemon = T)
