
##' @rdname middlewares 
##' @export
mw_session <-
    middleware("mw_session", handles = c("close", "ls-sessions", "clone"),
               fun =
                 function(h){
                     function(op, transport, session = .default_session_id, ...){
                         the_session <- sessions[[session]]
                         msg <- assoc(list(...), session = session)
                         switch(op,
                                "clone" = {
                                    new_session <- create_session(transport, the_session)
                                    transport$write(respfor(msg,
                                                            'new-session' = new_session[["id"]], 
                                                            status = c("done", "session-cloned")))
                                },
                                "close" = {
                                    sessions[[the_session[["id"]]]] <- NULL
                                    transport$write(respfor(msg,
                                                            status = c("done", "session-closed")))
                                },
                                "ls-sessions" = {
                                    transport$write(respfor(msg,
                                                            status = "done",
                                                            sessions = ls(sessions)))
                                },
                                h(op = op, transport = transport, session = session, ...))
                         }
                     })

                                
