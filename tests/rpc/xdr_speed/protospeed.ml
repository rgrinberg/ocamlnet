
let do_test title n f =
  Printf.printf "Test %s, count=%d: " title n;
  flush stdout;
  let t1 = Unix.gettimeofday() in
  for k = 1 to n do f() done;
  let t2 = Unix.gettimeofday() in
  Printf.printf "%5.2f seconds\n" (t2 -. t1);
  flush stdout
;;

do_test "test_001" 10000 Proto_testcase.test_001
;;
do_test "test_002" 10000 Proto_testcase.test_002
;;
