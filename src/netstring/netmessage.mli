type date = Netdate.t
type mailbox = Netaddress.mailbox
type address = Netaddress.t

type msg_id = string

class type header =
object

  (* Standard Header Fields - fields that are not present will
   * raise Not_found *)

  (* -------------------- RFC 822/2822 ------------------- *)

  (* Origination Date *)
  method date                : unit -> date

  (* Originator *)
  method from                : unit -> mailbox list
  method sender              : unit -> mailbox
  method reply_to            : unit -> address list

  (* Destination address *)
  method to_                 : unit -> address list
  method cc                  : unit -> address list
  method bcc                 : unit -> address list

  (* Identification *)
  method message_id          : unit -> msg_id
  method in_reply_to         : unit -> string list  (* phrase / msg-id *)
  method references          : unit -> string list  (* phrase / msg-id *)

  (* Informational *)
  method subject             : unit -> string
  method comments            : unit -> string
  method keywords            : unit -> string  

  (* Resent *) 
  method resent_date         : unit -> date
  method resent_from         : unit -> mailbox list
  method resent_sender       : unit -> mailbox
  method resent_to           : unit -> address list
  method resent_cc           : unit -> address list
  method recent_bcc          : unit -> address list
  method recent_message_id   : unit -> msg_id

  (* Trace *)
  method return_path         : unit -> mailbox
  method received            : unit -> ((string*string) list * date) list

  (* ---------------------- USENET ----------------------- *)

  method newsgroups          : unit -> string list
  method path                : unit -> string list
  method follwup_to          : unit -> string list
  method expires             : unit -> date
  method control             : unit -> string
  method distribution        : unit -> string list
  method organization        : unit -> string
  method summary             : unit -> string
  method approved            : unit -> mailbox
  method lines               : unit -> int

  (* ----------------------- MIME ------------------------ *)

  method mime_version        : unit -> string
  method content_type        : unit -> string
  method content_transfer_encoding : unit -> string
  method content_id          : unit -> msg_id
  method content_disposition : unit -> string
  method content_description : unit -> string
  
end
