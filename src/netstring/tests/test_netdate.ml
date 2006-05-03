open Fort
open Netdate
;;

let print_date x =
  if x.week_day > 0 then
    format "%c" x
  else
    format "%b %e %T %Y" x
;;

let equal_str   = expect_equal ~printer:(fun s -> s);;
let equal_date  = expect_equal ~printer:print_date;;
let equal_float = expect_equal ~printer:string_of_float;;

let t = 1000000000.0;;
let t_date =
    { year  =  2001;  month    =  9;  day    = 09;
      hour  =    01;  minute   = 46;  second = 40;
      zone  =  0*60;  week_day =  0;
    }
;;

expect_pass "epoch conversions"
begin fun () ->

  equal_date   t_date   (create t);
  equal_float  t        (since_epoch t_date);

end;;

expect_pass "RFC 822/1123 date parsing"
begin fun () ->

  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    21;  minute   = 30;  second =  7;
      zone  =  5*60;  week_day =  4;
    }
    (parse "Thu, 13 Jul 00 21:30:07 +0500");

  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    21;  minute   = 30;  second =  7;
      zone  =  5*60;  week_day =  4;
    }
    (parse "Thu, 13 Jul 2000 21:30:07 +0500");
end;;

expect_pass "RFC 850/1036 date parsing"
begin fun () ->

  equal_date
    { year  =  2001;  month    =  9;  day    =  9;
      hour  =     1;  minute   = 46;  second = 40;
      zone  =  0*60;  week_day =  0;
    }
    (parse "Sunday, 09-Sep-01 01:46:40 GMT");

end;;

expect_pass "ANSI C's asctime parsing"
begin fun () ->
  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    21;  minute   = 30;  second =  7;
      zone  =     0;  week_day =  4;
    }
    (parse "Thu Jul 13 21:30:07 2000");
end;;

expect_pass "Miscellaneous date parsing"
begin fun () ->

  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    10;  minute   = 35;  second = 4;
      zone  =     0;  week_day = -1;
    }
    (parse "13 Jul 00 10:35:04 AM");

  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    22;  minute   = 35;  second = 4;
      zone  =     0;  week_day = -1;
    }
    (parse "13 Jul 00 10:35:04 PM");

  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    22;  minute   = 35;  second = 4;
      zone  = -7*60;  week_day = -1;
    }
    (parse "13 Jul 00 10:35:04 PM MST");


  equal_float
    t
    (parse_epoch "Sunday, 09-Sep-01 01:46:40 +0000");

end;;

expect_pass "three digit years"
begin fun () ->
  equal_date
    { year  =  2000;  month    =  7;  day    = 13;
      hour  =    21;  minute   = 30;  second =  7;
      zone  =  5*60;  week_day =  4;
    }
    (parse "Thu, 13 Jul 100 21:30:07 +0500");

  equal_date
    { year  =  2070;  month    =  7;  day    = 13;
      hour  =    21;  minute   = 30;  second =  7;
      zone  =  5*60;  week_day =  4;
    }
    (parse "Thu, 13 Jul 170 21:30:07 +0500");
   
end;;

expect_pass "date formatting"
begin fun () ->

  equal_str "Sunday"                     (format "%A" t_date);
  equal_str "Sun"                        (format "%a" t_date);
  equal_str "September"                  (format "%B" t_date);
  equal_str "Sep"                        (format "%b" t_date);
  equal_str "20"                         (format "%C" t_date);
  equal_str "Sun Sep  9 01:46:40 2001"   (format "%c" t_date);
  equal_str "09/09/01"                   (format "%D" t_date);
  equal_str "09"                         (format "%d" t_date);
  equal_str " 9"                         (format "%e" t_date);
  equal_str "01"                         (format "%H" t_date);
  equal_str "Sep"                        (format "%h" t_date);
  equal_str "01"                         (format "%I" t_date);
  equal_str "252"                        (format "%j" t_date);
  equal_str " 1"                         (format "%k" t_date);
  equal_str " 1"                         (format "%l" t_date);
  equal_str "46"                         (format "%M" t_date);
  equal_str "09"                         (format "%m" t_date);
  equal_str "\n"                         (format "%n" t_date);
  equal_str "AM"                         (format "%p" t_date);
  equal_str "am"                         (format "%P" t_date);
  equal_str "01:46"                      (format "%R" t_date);
  equal_str "01:46:40 AM"                (format "%r" t_date);
  equal_str "40"                         (format "%S" t_date);
  equal_str "01:46:40"                   (format "%T" t_date);
  equal_str "\t"                         (format "%t" t_date);
  equal_str "36"                         (format "%U" t_date);
  equal_str "7"                          (format "%u" t_date);
  equal_str "0"                          (format "%w" t_date);
  equal_str "01:46:40"                   (format "%X" t_date);
  equal_str "09/09/01"                   (format "%x" t_date);
  equal_str "2001"                       (format "%Y" t_date);
  equal_str "01"                         (format "%y" t_date);
  equal_str "+0000"                      (format "%z" t_date);
  equal_str "%"                          (format "%%" t_date);

end;;

expect_pass "date construction"
begin fun () ->

  equal_str
    "Sun, 09 Sep 2001 01:46:40 +0000"
    (mk_mail_date t);
  equal_str
    "Sunday, 09-Sep-01 01:46:40 +0000"
    (mk_usenet_date t);

end;;

