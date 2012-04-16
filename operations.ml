(* file will contain file I/O and CamelJockey testing suite *)
open Test_framework
open Droplet
open Fountain
open Goblet

let transmit (message : string) : unit = 
    let f = new lt_fountain message 1 5 in
    let g = new lt_goblet f#output_droplet 5 in
    let rec helper () = 
        if g#check_complete
          then ()
          else (g#get_droplet f#output_droplet); 
          g#decode; g#print_progress;
          helper ()
    in
    helper ()

transmit "this is an awesome test" ;;

(* sample usage of the testing framework *)
(*
let sample_test = mk_expect_test
    (fun () -> test_function expected_result "message" ;;

run_test_set [sample_test;] "group message" ;;
*)
