open Droplet
open Fountain
open Goblet;;

if (Array.length Sys.argv) <> 5
then failwith "Usage: ./operations input_file output_destination piece_size 
                                                                     max_pieces"
else ()

let input_file = Sys.argv.(1)
let output_destination = Sys.argv.(2)
let piece_size = int_of_string Sys.argv.(3)
let max_pieces = int_of_string Sys.argv.(4)

let out_channel = open_out output_destination ;;

flush out_channel;;

(* read all the lines from a file, return a list of them as strings *)
(*
let rec input_chars inchan chars =
    try
      input_chars inchan ((String.make 1 (input_char inchan)) :: chars)
    with End_of_file -> List.rev chars
*)

let input_chars inchan chars =
  try 
    while true; do
      chars := (String.make 1 (input_char inchan) :: !chars)
    done; []
  with End_of_file -> List.rev !chars

(* condense the lines of a file into one big string *)
let condense_chars file =
    let ch       = open_in file in
    let chars    = input_chars ch (ref []) in
    let condense = String.concat "" chars in
      (close_in ch); condense

let message = condense_chars input_file

let f = new lt_fountain message piece_size max_pieces

let rec get_droplet () : droplet  = 
  let a = f#output_droplet in
  match a with
  |None   -> get_droplet ()
  |Some d -> d
    

let g = new lt_goblet (get_droplet ()) max_pieces

let rec transmit () : unit = 
    if g#check_complete
      then (g#print_progress; 
           (output_string out_channel g#get_message);
           (close_out out_channel);
           (print_string "\n"))
      else ((g#get_droplet f#output_droplet);
           g#decode;
           ignore(g#print_progress);
           transmit ()) ;;

transmit () ;;
