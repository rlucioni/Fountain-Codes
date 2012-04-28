open Fountain
open Droplet
open Goblet
open Distribution ;;

if (Array.length Sys.argv) <> 2
then failwith "Correct usage: ./probability string"
else ()

let message = Sys.argv.(1) ;;

Printf.printf "Piece Size: " ;;
let piece_size = read_int() ;;

Printf.printf "Choose a distribution (pois, norm, or unif): " ;;
let distro = read_line() ;;

let f =
match distro with
  | "unif" -> (Printf.printf "Max XOR'd Pieces: ";
    let max_pieces = read_int() in
    new lt_fountain message piece_size max_pieces)
  | "pois" -> (Printf.printf "Mean: ";
    let l = read_float() in
    new poisson_fountain l message piece_size 1)
  | "norm" -> (Printf.printf "Mean: ";
    let m = read_float() in
    Printf.printf "Variance: ";
    let v = read_float() in
    new normal_fountain m v message piece_size 1)
  | _ -> failwith "Not a valid distribution." ;;

let g =
match distro with
  | "unif" -> new lt_goblet f#output_droplet f#get_bound
  | "pois" -> new poisson_goblet f#get_mean f#output_droplet 1
  | "norm" -> new normal_goblet f#get_mean f#get_var f#output_droplet 1
  | _ -> failwith "Not a valid distribution."

let n = Printf.printf "# times to run: "; read_int() ;;

let rec transmit () : int = 
    if g#check_complete
      then g#num_used
      else ((g#get_droplet f#output_droplet);
           g#decode;
           ignore(g#get_message);
           transmit ()) ;;

let rec tester lst n : int list =
  if n = 0 then lst else tester (transmit()::lst) (n-1)

let rec stringify lst : string =
  match lst with
    | [] -> ""
    | hd::tl -> string_of_int hd ^ "," ^ (stringify tl) ;;

print_string (stringify (tester [] n))
