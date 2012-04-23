(* file will contain file I/O and CamelJockey testing suite *)
open Test_framework
open String_droplet
open String_fountain
open String_goblet;;

if (Array.length Sys.argv) <> 4
then failwith "Correct usage: ./string_operations string piece_size max_pieces"
else ()

let message = Sys.argv.(1)
let piece_size = int_of_string Sys.argv.(2)
let max_pieces = int_of_string Sys.argv.(3)

let f = new lt_fountain message piece_size max_pieces
let g = new lt_goblet f#output_droplet max_pieces

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
