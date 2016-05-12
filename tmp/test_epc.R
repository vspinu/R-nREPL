


cl <- client(con <- connect_epc(4005, verbose = T))

epc_handler()(nREPL:::transport_print(), epc_msg(op = q("echo"), id = 2, args = list("abc")))

encode(epc_handler()(transport_return(), epc_msg(op = q("echo"), id = 2, args = list(123, 123))))
encode(epc_handler()(transport_return(), epc_msg(op = q("echo"), id = 2, args = list("abc"))))

encode(epc_handler()(transport_return(), epc_msg(op = q("methods"), id = 2, args = list("abc"))))

start_epc(4005)
