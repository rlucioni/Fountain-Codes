open Droplet 
exception TODO
 (* This is a type for metadrop which is a droplet that had its seed decoded into metadata
  * number_chunks is how many chunks went into the droplet
  * pieces_list is the chunks that went into it
  * data is obvious   *)
let string_of_int_list (lst: int list) : string = 
   List.fold_right (fun x y -> (string_of_int x)^", "  ^ y ) lst ""

type metadrop = { number_chunks : int ; pieces_list : int list ; contents : string  }

(* a Goblet, used to collect Droplets and reconstruct the original data *)
class type goblet =
object
    (* total number of pieces in the original file *)
    val mutable totalPieces : int
 
    (* a list of all metadrop in the goblet  *)
    val mutable all_metadrops : metadrop list 
    
   (* a list of metadrops that are made of one chunk *) 
    val mutable solved_metadots : metadrop list
    
    (* data structure representing the part of the message we have decoded so
     * far *) (* string for current implementation  *)
    val mutable message :string 

    (* number that shows how much of the file we have decoded, in pieces *)
    val mutable counter : int

    (* takes the droplet d  and returns the metadrop  *)
    method get_metadrop: metadrop

    (* takes a droplet runs get_metadrop and adds it to the all_metadrops *)
    method get_droplet : droplet -> unit 

    (* runs on all_metadrops and trys to decode it  *)
    method decode: unit

    (* takes two metadrops as arguments,  xors the data and xors the 
     * pieces_list to produce a new metadrop  *)
    method meta_d_xor: metadrop

    (* return however much we have decoded of the original message *)
    method get_message : string 

    (* prints: total pieces, all_metadrops, message and  counter for debugging *)
    method print_progress : unit

    (* compares counter and total_pieces, checking to see if we are done
     * decoding (i.e., counter = total_pieces) *)
    method check_complete: bool
end

(*
 * just doing chars right now 
 *  total pieces is string length 

*)

class lt_goblet (d: droplet) : goblet =
object
    val mutable totalPieces = d#get_contents.total_pieces
    (* initiates the message to a string of stars of the correct length
     * for fun  *)
    val mutable message = String.make (d#get_contents.total_pieces) '*'
    val mutable all_metadrops = [] 
    val mutable counter = 0
    

    method get_metadrop: metadrop = raise TODO

    method decode_drop: unit = raise TODO

    method meta_d_xor: metadrop  = raise TODO

    method get_message  = raise TODO

    method print_progress : unit  = raise TODO
 (*    let string_of_metadrop (m : metadrop) : string = (
       "{ number_chunks :" ^ (string_of_int m.number_chunks) ^
       "; pieces_list : [" ^ (string_of_int_list m.pieces_list) ^
       "] ; contents : " ^ (m.contents) ^ " }" )
     in 
     let string_of_metadrop_list (mlst: metadrop list) : string= 
      ( List.fold_right (fun x y -> ((string_of_metadrop x)^ " \n" ^ y)) mlst "" )
     in 
      " totalPieces =" ^ (string_of_int totalPieces) ^ "\n 
        message = " ^ message ^ "\n
        all_metadrops = \n ^ "
        string_of_metadrop_list all_metadrops ^ " \n
        counter = " ^ string_of_int counter ^ "\n"
 *)
    method check_complete : bool = counter = totalPieces
    
end
