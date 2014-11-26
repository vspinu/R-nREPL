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

