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

let in_channel  = open_in input_file ;;
let out_channel = open_out output_destination ;;

flush out_channel;;

let rec file_to_string () : string =
    (input_line in_channel) (* ^ (file_to_string ()) *)

let message = file_to_string ()


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
