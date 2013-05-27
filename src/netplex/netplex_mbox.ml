(* $Id$ *)

open Netplex_types

let plugin =
  (object
      method required =
               [ Netplex_sharedvar.plugin;
                 Netplex_semaphore.plugin
               ]
      method program = failwith "Netplex_mbox#program"
      method ctrl_added _ = ()
      method ctrl_unplugged _ = ()
      method ctrl_receive_call _ _ _ _ _ = ()
      method ctrl_container_finished _ _ _ = ()
    end : plugin
  )


module type MBOX = sig
  type t
  type mbox

  val create : string -> mbox
  val send : mbox -> t -> unit
  val receive : mbox -> t
end


module Make_mbox_type(T:Netplex_cenv.TYPE) = struct
  type t = T.t
  type mbox = string

  module Message_var = Netplex_sharedvar.Make_var_type(T)

  let create name =
    let box_name = "netplex." ^ name ^ ".box" in
    let sem1_name = "netplex." ^ name ^ ".sem1" in
    let sem2_name = "netplex." ^ name ^ ".sem2" in
    let box_created =
      Netplex_sharedvar.create_var ~enc:true box_name in
    if box_created then (
      (* init semaphores *)
      ignore(Netplex_semaphore.create sem1_name 0L);
      ignore(Netplex_semaphore.create sem2_name 1L);
      (* Message_var.set box_name Null *)
    )
    else
      ignore(Netplex_sharedvar.wait_for_enc_value box_name);
    name
    
  let send name (msg : t) =
    let box_name = "netplex." ^ name ^ ".box" in
    let sem1_name = "netplex." ^ name ^ ".sem1" in
    let sem2_name = "netplex." ^ name ^ ".sem2" in
    ignore(Netplex_semaphore.decrement ~wait:true sem2_name);
    Message_var.set box_name msg;
    ignore(Netplex_semaphore.increment sem1_name)

  let receive name =
    let box_name = "netplex." ^ name ^ ".box" in
    let sem1_name = "netplex." ^ name ^ ".sem1" in
    let sem2_name = "netplex." ^ name ^ ".sem2" in
    ignore(Netplex_semaphore.decrement ~wait:true sem1_name);
    let msg = Message_var.get box_name in
    ignore(Netplex_semaphore.increment sem2_name);
    msg
end
