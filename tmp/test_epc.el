(require 'epc)

(setq epc:debug-out t)
(setq deferred:debug-on-signal t)

(defun epc-connect (port)
  (epc:init-epc-layer
   (make-epc:manager :server-process nil
                     :commands "no commands"
                     :title "test"
                     :port port
                     :connection (epc:connect "localhost" port))))

(progn
  ;; (setq epc (epc-connect 8888))
  (setq epc (epc-connect 4005))
  (deferred:$
    (epc:call-deferred epc 'echo '(10 20))
    (deferred:nextc it 
      (lambda (x) (message "Return : %S" x)))))


(deferred:$
  (epc:call-deferred epc 'methods nil)
  (deferred:nextc it 
    (lambda (x) (message "Return : %S" x))))

(deferred:$
  (epc:call-deferred epc 'echo 10)
  (deferred:nextc it 
    (lambda (x) (message "Return : %S" x))))

(message "%S" (epc:sync epc (epc:query-methods-deferred epc)))

(deferred:$
  (epc:call-deferred epc 'add '(10 40))
  (deferred:nextc it 
    (lambda (x) (message "Return : %S" x))))

(epc:stop-epc epc)

(require 'epcs)
(let ((connect-function
       (lambda (mngr) 
         (epc:define-method mngr 'echo (lambda (&rest x) x) "args" "just echo back arguments.")
         (epc:define-method mngr 'add '+ "args" "add argument numbers.")
         )) server-process)

  (setq server-process (epcs:server-start connect-function))
  (sleep-for 10)
  (epcs:server-stop server-process))
