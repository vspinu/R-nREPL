An implementation of [nREPL](https://github.com/clojure/tools.nrepl) client and
server for [R](http://www.r-project.org/).


## Installation ##

```R
library(devtools)
install_github("R-nREPL", "vspinu")
```

## Communicate with Clojure ##

Start `lein repl` and pick the port number from the startup message.

At R's command prompt:

```R

> library("nREPL")
> tr <- connect(port = 37616) 
> cl <- client(tr)  # create the client

## send sync requests, returns a list of responses
> sync_request(cl, op = "eval", code = "(* 4 5)") 
[[1]]
BENDICT:
 $ id     : num 2
 $ ns     : chr "user"
 $ session: chr "dcd9617e-d5c1-4467-a4f0-a63a971c89df"
 $ value  : chr "20"

[[2]]
BENDICT:
 $ id     : num 2
 $ session: chr "dcd9617e-d5c1-4467-a4f0-a63a971c89df"
 $ status : chr "done"

## combine all responses into one list
> sync_request0(cl, op = "eval", code = "(def a 10)")
BENDICT:
 $ id     : num 6
 $ ns     : chr "user"
 $ session: chr "34ac4de0-fe97-4559-ae4a-978c3ea7878d"
 $ status : chr "done"
 $ value  : chr "#'user/a"

```

## Communicate with R ##

Start two R processes, one for server and one for client. In the server process
invoke `start_server(port = 4005)`. In the client process invoke the request
commands:


```R

> tr <- connect(port = 4005) # create the transport
> cl <- client(tr)  # create the client

> sync_request(cl, op = "describe")
[[1]]
BENDICT:
 $ id      : chr "62"
 $ ops     :List of 3
  ..$ mw_describe: list()
  ..$ mw_eval    : list()
  ..$ mw_session : list()
 $ session : chr "R/default"
 $ status  : chr "done"
 $ versions:List of 2
  ..$ nrepl:List of 4
  .. ..$ incremental   : num 1
  .. ..$ major         : num 0
  .. ..$ minor         : num 0
  .. ..$ version-string: chr "0.0.1"
  ..$ R    :List of 4
  .. ..$ incremental   : num 1
  .. ..$ major         : num 3
  .. ..$ minor         : num 1
  .. ..$ version-string: chr "R version 3.1.1 Patched (2014-10-18 r66793)"

> sync_request0(cl, op = "eval", code = "4 + 5")
BENDICT:
 $ id     : chr "59"
 $ session: chr "R/default"
 $ status : chr [1:2] "eval-value" "done"
 $ value  : chr "9"

> sync_request(cl, op = "eval", code = "4 + 5; warning('warn 1'); message('mes 2'); stop('err 3')")
[[1]]
BENDICT:
 $ id     : chr "61"
 $ session: chr "R/default"
 $ status : chr "eval-value"
 $ value  : chr "9"

[[2]]
BENDICT:
 $ id     : chr "61"
 $ session: chr "R/default"
 $ status : chr "eval-warning"
 $ warning: chr "warn 1"

[[3]]
BENDICT:
 $ id     : chr "61"
 $ message: chr "mes 2\n"
 $ session: chr "R/default"
 $ status : chr "eval-message"

[[4]]
BENDICT:
 $ error  : chr "err 3"
 $ id     : chr "61"
 $ session: chr "R/default"
 $ status : chr "eval-error"

[[5]]
BENDICT:
 $ id     : chr "61"
 $ session: chr "R/default"
 $ status : chr "done"

```