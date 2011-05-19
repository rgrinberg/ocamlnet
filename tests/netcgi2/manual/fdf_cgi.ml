(** This scripts receive the data from a PDF form and sends back the
    current date and time to update the form.

    The form is created by compiling fdf_form.tex with pdflatex.  The
    form must contain the fields "date", "time" and "args".  You need
    to update \submiturl in pdf_form.tex to point to the URL where
    this cgi script is made accessible.
*)

open Netcgi
open Printf

let text = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ()

let config = {
  default_config with
    permitted_http_methods = [`GET; `HEAD; `POST; `DELETE; `PUT];
    permitted_input_content_types = [ "application/vnd.fdf" ];
}


let send_fdf (cgi:cgi) =
  cgi#set_header ~content_type:"application/vnd.fdf" ();
  let now = Netdate.create (Unix.time()) in
  let date = Netdate.format "%a, %d %B %Y" now
  and time = Netdate.format "%T" now in
  let args = String.escaped(cgi#argument_value "BODY") in
  cgi#out_channel#output_string ("\
%FDF-1.2
1 0 obj
<<
/FDF << /Fields [
<< /V (" ^ date ^ ")/T (date) >>
<< /V (" ^ time ^ ")/T (time) >>
<< /V (" ^ args ^ ")/T (args) >>
  ] >>
>>
endobj
trailer
<<
/Root 1 0 R
>>
%%EOF");
  cgi#out_channel#commit_work()


let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~config ~output_type:(`Transactional buffered) send_fdf
