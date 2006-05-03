open Fort
open Netaddress;;

let id x = x

let list_printer f l =
   "[" ^ (String.concat "; " (List.map f l)) ^ "]"

let spec_printer (local_part, domain) =
  match domain with
    | None -> local_part
    | Some d -> local_part ^ "@" ^ d
;;

let eq_mailbox x y =
  expect_equal_app ~printer:id ~msg:"name"
    x#name ()
    y#name ();
  expect_equal ~printer:(list_printer id) ~msg:"route"
    x#route y#route;
  expect_equal ~printer:spec_printer ~msg:"spec"
    x#spec y#spec;
;;

let eq_group x y=
  expect_equal ~printer:id
    x#name y#name;
  List.iter2 eq_mailbox x#mailboxes y#mailboxes
;;

let address_pass name (res : Netaddress.t list) s =
  expect_pass name (fun () ->
    List.iter2 (fun x y ->
      match x,y with
	| `Mailbox x, `Mailbox y -> eq_mailbox x y
	| `Mailbox _, _          -> fail "expecting mailbox but found group"
	| `Group x, `Group y     -> eq_group x y
	| `Group _, _            -> fail "expecting group but found mailbox"
    ) res (parse s)
  )
;;

address_pass "empty list"
  []
  ""
;;

address_pass "address spec"
  [`Mailbox
    (new mailbox [] ("Neuman", Some "BBN-TENEXA"))]
  "Neuman@BBN-TENEXA"
;;

address_pass "address spec in brackets"
  [`Mailbox
    (new mailbox [] ("boss", Some "nil.test"))]
  "<boss@nil.test>"
;;

address_pass "personal name 1"
  [`Mailbox
    (new mailbox ~name:"Alfred Neuman" [] ("Neuman", Some "BBN-TENEXA"))]
  "Alfred Neuman <Neuman@BBN-TENEXA>"
;;

address_pass "personal name 2"
  [`Mailbox
    (new mailbox ~name:"Mary Smith" [] ("mary", Some "x.test"))]
  "Mary Smith <mary@x.test>"
;;

address_pass "quoted personal name 1"
  [`Mailbox
    (new mailbox ~name:"George, Ted" [] ("Shared", Some "Group.Arpanet"))]
  "\"George, Ted\" <Shared@Group.Arpanet>"
;;

address_pass "quoted personal name 2"
  [`Mailbox
    (new mailbox ~name:"Joe Q. Public" []
      ("john.q.public", Some "example.com"))]
  "\"Joe Q. Public\" <john.q.public@example.com>"
;;

address_pass "stripping comments"
  [`Mailbox
    (new mailbox [] ("Wilt.Chamberlain", Some "NBA.US"))]
  "Wilt . (the  Stilt) Chamberlain@NBA.US"
;;

address_pass "nested quotes"
  [`Mailbox
    (new mailbox ~name:"Giant; \"Big\" Box" []
     ("sysservices", Some "example.net"))]
  "\"Giant; \\\"Big\\\" Box\" <sysservices@example.net>"
;;

address_pass "empty group"
  [`Group
    (new group "Undisclosed recipients" [])]
  "Undisclosed recipients:;"
;;

address_pass "simple group 1"
  [`Group
    (new group "group"
     [new mailbox [] ("user", Some "domain")])]
  "group: user@domain;"
;;

address_pass "simple group 2"
  [`Group
    (new group "A Group"
    [new mailbox ~name:"Chris Jones" [] ("c", Some "a.test");
     new mailbox                     [] ("joe", Some "where.test");
     new mailbox ~name:"John"        [] ("jdoe", Some "one.test");])]
  "A Group:Chris Jones <c@a.test>,joe@where.test,John <jdoe@one.test>;"
;;

address_pass "many groups"
  (* This test is a slight variation of an example shown in RFC-822. The
     example there has a syntax error. *)
  [`Group
    (new group "Gourmets"
     [new mailbox ~name:"Pompous Person" [] ("WhoZiWhatZit", Some "Cordon-Bleu");
      new mailbox [] ("Childs", Some "WGBH.Boston");
      new mailbox [] ("Galloping Gourmet", Some "ANT.Down-Under");
      new mailbox [] ("Cheapie", Some "Discount-Liquors")]);
   `Group
    (new group "Cruisers"
     [new mailbox [] ("Port", Some "Portugal");
      new mailbox [] ("Jones", Some "SEA");]);
   `Mailbox
    (new mailbox [] ("Another", Some "Somewhere.SomeOrg"))]
  "Gourmets:  Pompous Person <WhoZiWhatZit@Cordon-Bleu>, Childs@WGBH.Boston, \"Galloping Gourmet\"@ANT.Down-Under (Australian National Television), Cheapie@Discount-Liquors;, Cruisers:  Port@Portugal, Jones@SEA;, Another@Somewhere.SomeOrg"
;;

address_pass "other oddities 1"
  [`Mailbox
    (new mailbox ~name:"Pete" [] ("pete", Some "silly.test"))]
  "Pete(A wonderful \) chap) <pete(his account)@silly.test(his host)>"
;;

address_pass "other oddities 2"
  [`Group
    (new group "A Group"
     [new mailbox ~name:"Chris Jones" [] ("c", Some "public.example");
      new mailbox [] ("joe", Some "example.org");
      new mailbox ~name:"John" [] ("jdoe", Some "one.test")])]
  "A Group(Some people)
     :Chris Jones <c@(Chris's host.)public.example>,
         joe@example.org,
  John <jdoe@one.test> (my dear friend); (the end of the group)"
;;

address_pass "other oddities 3"
  [`Group
    (new group "Undisclosed recipients" [])]
  "(Empty list)(start)Undisclosed recipients  :(nobody(that I know))  ;"
;;

address_pass "obsolete - missing quotes for '.'"
  [`Mailbox
    (new mailbox ~name:"Joe Q. Public" [] 
      ("john.q.public", Some "example.com"))]
  "Joe Q. Public <john.q.public@example.com>"
;;

address_pass "obsolete - route address, two commas in sequence, extra space"
  [`Mailbox
    (new mailbox ~name:"Mary Smith" ["machine.tld"]
      ("mary", Some "example.net"));
   `Mailbox
    (new mailbox [] ("jdoe", Some "test.example"))]
  "Mary Smith <@machine.tld:mary@example.net>, , jdoe@test   . example"
;;

address_pass "missing domain"
  [`Mailbox
    (new mailbox ~name:"Mail Delivery Subsystem" []
     ("MAILER-DAEMON", None))]
  "Mail Delivery Subsystem <MAILER-DAEMON>"
;;

