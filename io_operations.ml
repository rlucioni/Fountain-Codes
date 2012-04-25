open Test_framework
open String_droplet
open String_fountain
open String_goblet;;

if (Array.length Sys.argv) <> 5
then failwith "Usage: ./io_operations input_file output_destination piece_size 
                                                                     max_pieces"
else ()

let input_file = Sys.argv.(1)
let output_destination = Sys.argv.(2)
let piece_size = int_of_string Sys.argv.(3)
let max_pieces = int_of_string Sys.argv.(4)

let out_channel = open_out output_destination ;;

flush out_channel;;

(* read all the lines from a file, return a list of them as strings *)
let rec input_lines inchan lines =
    try
      input_lines inchan ((input_line inchan) :: lines)
    with End_of_file -> List.rev lines 

(* condense the lines of a file into one big string *)
let condense_lines file =
    let ch       = open_in file in
    let lines    = input_lines ch [] in
    let condense = String.concat "\n" lines in
      close_in ch; condense

let message = condense_lines input_file

let f = new lt_fountain message piece_size max_pieces
let g = new lt_goblet f#output_droplet max_pieces

let rec transmit () : unit = 
    if g#check_complete
      then (g#print_progress; 
           (output_string out_channel g#return_message);
           (close_out out_channel))
      else ((g#get_droplet f#output_droplet);
           g#decode;
           ignore(g#print_progress);
           transmit ()) ;;

transmit () ;;
