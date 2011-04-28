#use "topfind";;
#require "netstring";;
#directory "fort";;
#load "fort.cmo";;

open Fort
open Nethtml
;;

expect_pass "tag minimization" (fun () ->
  let in_s = "<table>
  <thead>
    <tr>
      <td>head 1
      <td>head 2
  <tbody>
    <tr>
      <td>line 1
      <td>line 2
</table>" 
  and expect_s = "<table>
  <thead>
    <tr>
      <td>head 1
      </td><td>head 2
  </td></tr></thead><tbody>
    <tr>
      <td>line 1
      </td><td>line 2
</td></tr></tbody></table>"
  in

  let in_ch = new Netchannels.input_string in_s in
  let buffer = Buffer.create 128 in
  let out_ch = new Netchannels.output_buffer buffer in
  write out_ch (parse in_ch);
  let out_s = Buffer.contents buffer in
  expect_equal ~printer:(fun x -> x) expect_s out_s
);
