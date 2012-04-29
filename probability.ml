open Fountain
open Droplet
open Goblet
open Distribution ;;

if (Array.length Sys.argv) <> 2
then failwith "Correct usage: ./probability input_file"
else ()

let input_file = Sys.argv.(1) ;;

(* read all the lines from a file, return a list of them as strings *)
let rec input_chars inchan chars =
    try
      input_chars inchan ((String.make 1 (input_char inchan)) :: chars)
    with End_of_file -> List.rev chars

(* condense the lines of a file into one big string *)
let condense_chars file =
    let ch       = open_in file in
    let chars    = input_chars ch [] in
    let condense = String.concat "" chars in
      (close_in ch); condense

let message = condense_chars input_file ;;

Printf.printf "Piece Size: " ;;
let piece_size = read_int() ;;

Printf.printf "Choose a distribution (pois, norm, or unif): " ;;
let distro = read_line() ;;

let max_pieces = ref 0
let mean = ref 0.
let var = ref 0. ;;

match distro with
  | "unif" -> Printf.printf "Max XOR'd Pieces: ";
    max_pieces := read_int()
  | "pois" -> Printf.printf "Mean: ";
    mean := read_float()
  | "norm" -> Printf.printf "Mean: ";
    mean := read_float() ;
    Printf.printf "Variance: ";
    var := read_float()
  | _ -> failwith "Not a valid distribution." ;;

let n = Printf.printf "# times to run: "; read_int() ;;

(* gets and then strips the option from a droplet *)
let rec get_droplet f : droplet  = 
  let a = f#output_droplet in
  match a with
  |None   -> get_droplet ()
  |Some d -> d

let rec transmit (f:fountain) (g:goblet) : int =
    if g#check_complete
    then (*(print_string (g#get_message ^ "\n");*) g#num_used
      else ((g#get_droplet (get_droplet f));
           g#decode;
           ignore(g#get_message);
           transmit f g) ;;

let f_initialize () : fountain =
  match distro with
    | "unif" -> new lt_fountain message piece_size !max_pieces
    | "pois" -> new poisson_fountain !mean message piece_size 0
    | "norm" -> new normal_fountain !mean !var message piece_size 0
    | _ -> failwith "Not a valid distribution."

let g_initialize (f:fountain) =
  match distro with
    | "unif" -> new lt_goblet (get_droplet f) f#get_bound
    | "pois" -> new poisson_goblet f#get_mean (get_droplet f) 1
    | "norm" -> new normal_goblet f#get_mean f#get_var (get_droplet f) 1
    | _ -> failwith "Not a valid distribution."

let transmitter () =
  let f = f_initialize () in
  let g = g_initialize f in
  transmit f g

let rec tester lst n : int list =
  if n = 0 then lst else tester (transmitter()::lst) (n-1)

let rec stringify lst : string =
  match lst with
    | [] -> ""
    | hd::tl -> string_of_int hd ^ "," ^ (stringify tl);;

print_string ("\nFile length: " ^ (string_of_int ((String.length message) / piece_size)) ^ "\n") ;;
print_string ((stringify (tester [] n)) ^ "\n")
