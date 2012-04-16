(* file will contain file I/O and CamelJockey testing suite *)
open Test_framework
open Droplet
open Fountain
open Goblet

let message = "I love Theresa a whole lot!"

let f = new lt_fountain message 1 10
let g = new lt_goblet f#output_droplet 10

let rec transmit () : unit = 
    if g#check_complete
      then g#print_progress
      else ((g#get_droplet f#output_droplet); 
           g#decode; ignore(g#get_message); transmit ()) ;;

transmit () ;;

(* sample usage of the testing framework *)
(*
let sample_test = mk_expect_test
    (fun () -> test_function expected_result "message" ;;

run_test_set [sample_test;] "group message" ;;
*)
