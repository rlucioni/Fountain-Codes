(* file will contain file I/O and CamelJockey testing suite *)
open Test_framework
open Droplet
open Fountain
open Goblet

let message = Sys.argv.(1)

let f = new lt_fountain message 1 5
let g = new lt_goblet f#output_droplet 5

let rec transmit () : unit = 
    if g#check_complete
      then g#print_progress
      else ((g#get_droplet f#output_droplet); 
           g#decode;
           ignore(g#get_message); 
           transmit ()) ;;

transmit () ;;


(* Tests for transmit *)
(*
let test1 = mk_expect_test
    (fun () -> transmit ()) "This is a test." "This is a test." ;;

run_test_set [test1;] "Transmit Tests" ;;
*)
