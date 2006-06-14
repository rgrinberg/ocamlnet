open Mount_clnt.MOUNTPROG.MOUNTVERS;;
open Unix;;
let s = socket PF_INET SOCK_DGRAM 0;;
bind s (ADDR_INET(inet_addr_any, 999));;
connect s (ADDR_INET(inet_addr_of_string "192.168.0.13",853));;
let c = create_client (Rpc_client.Descriptor s) Rpc.Udp;;
let m = Rpc_auth_sys.client_auth_method ~identity:(`This_user(0,0,[||],"ice")) ();;
Rpc_client.set_auth_methods c [m];;
let r = mountproc_mnt c "/";;
