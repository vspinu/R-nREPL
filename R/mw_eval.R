
eval_handler <- function(msg, transport = transport_bencode){
    gr_id <- 0L
    new_output_handler(
        source = function(x) NULL,
        text = function(x){
            transport$write(respfor(msg, out = x, status = "eval-out"))
        },
        value = function(x){
            transport$write(respfor(msg, value = x, status = "eval-value"))
        }, 
        message = function(x){
            transport$write(respfor(msg, message = x$message, status = "eval-message"))
        },
        warning = function(x){
            transport$write(respfor(msg, warning = x$message, status = "eval-warning"))
        },
        error = function(x){
            transport$write(respfor(msg, error = x$message, status = "eval-error"))
        },
        graphics = function(x){
            transport$write(respfor(msg, graphics = sprintf("[%d]", gr_id),
                                    status = "eval-graphics" ))
            gr_id <<- gr_id + 1L
        })
}

##' @rdname middlewares
##' @export
mw_eval <-
    middleware("mw_eval", handles = c("eval"),
               fun =
                 function(h){
                     function(op, transport, code = NULL, ...){
                         if(op == "eval"){
                             msg <- list(...)
                             if( is.null(code) )
                                 transport$write(
                                     respfor(msg, status = c("error", "no-code", "done")))
                             else {
                                 evaluate(code, new_device = F, stop_on_error = 1L,
                                          output_handler =
                                            eval_handler(msg, transport = transport))
                                 transport$write(respfor(msg, status = "done"))
                             }
                         } else
                             h(op = op, transport = transport, ...)
                     }
                 })


### DO NOT DELETE

## txt <- "
##     a <- 343
##     b <- a + 1
##     warning('first warning')
##     print(b)
##     message('first message')
##     stop('first error')
##     message('second warning')
##     plot(1:2)
##     lines(1:2)
##     a"

## png(type = "Xlib")
## str(evaluate("plot(1:2)", new_device=F, 
##              output_handler =
##                new_output_handler(graphics = function(x) print("here"))))
## dev.off()

## tmsg <- list(id = 12, session = "R/test")
## tcon <- textConnection("toutput", open = "w")
## x <- evaluate(txt, new_device = F, stop_on_error = 1, debug = T, 
##               output_handler = eval_handler(tmsg, transport_print(tcon)))
## close(tcon)
## cat(toutput, sep = "\n")

