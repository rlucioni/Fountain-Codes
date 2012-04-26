open String_fountain
open String_droplet
open String_goblet 
open Distribution ;;

if (Array.length Sys.argv) <> 2
then failwith "Correct usage: ./string_operations string"
else ()

let message = Sys.argv.(1) ;;

Printf.printf "Piece Size: " ;;
let piece_size = read_int() ;;

Printf.printf "Max XOR'd Pieces: " ;;
let max_pieces = read_int() ;;

Printf.printf "Choose a distribution (pois, norm, or unif): " ;;
let distro = read_line() ;;

let f =
match distro with
  | "unif" -> new lt_fountain message piece_size max_pieces
  | "pois" -> (Printf.printf "Mean: ";
    let l = read_float() in
    new poisson_fountain l message piece_size max_pieces)
  | "norm" -> (Printf.printf "Mean: ";
    let m = read_float() in
    Printf.printf "Variance: ";
    let v = read_float() in
    new normal_fountain m v message piece_size max_pieces)
  | _ -> failwith "Not a valid distribution." ;;
(*
let g = new lt_goblet f#output_droplet max_pieces ;;

let rec transmit () : unit = 
    if g#check_complete
      then g#print_progress
      else ((g#get_droplet f#output_droplet);
           g#decode;
           ignore(g#get_message);
           transmit ()) 

transmit () 
*)
