##' High level client requests.
##'
##' These are high level wrapers around \code{\link{sync_request}} to request
##' the operations provided by default nREPL middleware.
##'
##' @name client-requests
NULL

##' @rdname client-requests
##' @param client Client function created by a call to \code{\link{client}}
##' constructor.
##' @param clone Id of the session to clone.
##' @export 
new_session <- function(client, clone = NULL){
    out <-
        if(is.null(clone))
            sync_request(client, op = "clone")
        else
            sync_request(client, op = "clone", session = clone)
    out <- combine_responses(out)
    if(!is.null(session <- out[["new-session"]]))
        session
    else
        stop("Could not open new session, :clone response:", out)
}

