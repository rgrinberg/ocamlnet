(* $Id$ *)

let nt_hostbased_service =
  [| 1; 3; 6; 1; 5; 6; 2 |]

let nt_user_name =
  [| 1; 2; 840; 113554; 1; 2; 1; 1 |]

let nt_machine_uid_name =
  [| 1; 2; 840; 113554; 1; 2: 1; 2 |]

let nt_string_uid_name =
  [| 1; 2; 840; 113554; 1; 2; 1; 3 |]

let nt_anonymous =
  [| 1; 3; 6; 1; 5; 6; 3 |]

let nt_export_name =
  [| 1; 3; 6; 1; 5; 6; 4 |]
