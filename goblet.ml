open Droplet 
open Random
exception TODO
 (* This is a type for metadrop which is a droplet that had its seed decoded into metadata
  * number_chunks is how many chunks went into the droplet
  * pieces_list is the chunks that went into it
  * data is obvious   *)
let string_of_int_list (lst: int list) : string = 
   List.fold_right (fun x y -> (string_of_int x)^", "  ^ y ) lst ""

 let lst_fixer (lst:int list) : int list =
 let lstSorted = List.sort compare lst in 
  let rec duplicate_remover (lst: int list) : int list = 
    ( match lst with 
      |[] -> []
      |hd::[] -> [hd]
      |hd::tl -> if hd = List.hd tl then duplicate_remover (List.tl tl)
         else hd::duplicate_remover tl)
       in  duplicate_remover lstSorted

let int_list_xor (lst1: int list) (lst2: int list) : int list =
  let lst = lst1@lst2 in 
   lst_fixer lst

type metadrop = { number_chunks : int ; pieces_list : int list ; contents : char (*string*)  }

(* a Goblet, used to collect Droplets and reconstruct the original data *)
class type goblet =
object
    (* total number of pieces in the original file *)
    val mutable totalPieces : int
 
    (* a list of all metadrop in the goblet  *)
    val mutable all_metadrops : metadrop list 
    
   (* a list of metadrops that are made of one chunk *) 
    val mutable solved_metadrops : metadrop list
    
    (* data structure representing the part of the message we have decoded so
     * far *) (* string for current implementation  *)
    val mutable message :string 

    (* number that shows how much of the file we have decoded, in pieces *)
    val mutable counter : int

    (* takes the droplet d  and returns the metadrop  *)
    method get_metadrop: droplet -> int -> metadrop

    (* takes a droplet runs get_metadrop and adds it to the all_metadrops *)
    method get_droplet : droplet -> unit 

    (* runs on all_metadrops and trys to decode it  *)
    method decode: unit

    (* return however much we have decoded of the original message *)
    method get_message : string 

    (* prints: total pieces, all_metadrops, message and  counter for debugging *)
    method print_progress : unit

    (* compares counter and total_pieces, checking to see if we are done
     * decoding (i.e., counter = total_pieces) *)
    method check_complete: bool

    method meta_d_xor: metadrop -> metadrop -> metadrop
    method metadrop_fixer: metadrop -> metadrop
    method meta_simplify: metadrop -> metadrop -> metadrop 
    method get_all_metadrops: metadrop list
end

(*
 * just doing chars right now 
 *  total pieces is string length 

*)

class lt_goblet (d: droplet) (bound: int) : goblet =
object (self)
    val mutable totalPieces = d#get_contents.total_pieces
    (* initiates the message to a string of stars of the correct length
     * for fun  *)
    val mutable message = String.make (d#get_contents.total_pieces) '_'
    val mutable all_metadrops = [] 
    val mutable solved_metadrops = []
    val mutable counter = 0
    

    method get_metadrop (d:droplet) (bound:int)  : metadrop = 
      let drop = d#get_contents in 
      let seed = drop.seed in 
       (*Char.escaped changes a char to a string this is for if we want to encode 
        * with string later  *) 
      let contents =(* Char.escaped*) (drop.data) in
     (* let total_pieces = drop.total_pieces *)
      init seed; let num_chunks = (int bound) +1 in 
	 let rec get_int_list (n:int) : int list = 
          ( if n > 1  then int totalPieces :: get_int_list (n-1)
           else [(int totalPieces)] )
         in
      self#metadrop_fixer {number_chunks = num_chunks; 
                           pieces_list = (get_int_list num_chunks);
                           contents}
     
    
    method get_droplet (d: droplet) : unit = 
      let metad = (self#get_metadrop d bound) in 
     all_metadrops <- (metad::all_metadrops);
     ()

    method decode: unit = raise TODO

    method(* private*) metadrop_fixer (m:metadrop) : metadrop = 
      let lst_fixer (lst:int list) : int list =
       (let lstSorted = List.sort compare lst in 
         let rec duplicate_remover (lst: int list) : int list = 
           (match lst with 
            |[] -> []
            |hd::[] -> [hd]
            |hd::tl -> if hd = List.hd tl then duplicate_remover (List.tl tl)
         else hd::duplicate_remover tl)
       in  duplicate_remover lstSorted )
      in 
   let lst = m.pieces_list in 
   let lst2 = lst_fixer lst in 
    { number_chunks = m.number_chunks ; pieces_list = lst2 ; contents = m.contents}

    method(* private*) meta_d_xor (m1:metadrop) (m2:metadrop) : metadrop  = 
      let lst = m1.pieces_list@m2.pieces_list in 
      let contents = char_of_int( (lxor) (int_of_char m1.contents) (int_of_char m2.contents))
      in self#metadrop_fixer {number_chunks=m1.number_chunks; pieces_list = lst ; contents }
  
    method singleKnockout (solved_meta:metadrop) (m:metadrop) : metadrop = raise TODO

   method meta_simplify (m1:metadrop) (m2:metadrop) : metadrop = raise TODO
    (*   if m1.pieces_list = [] then m2 else if m2.pieces_list = [] then m1 else
       if m1.pieces_list = m2.pieces_list then m1 else*)
(*	 let simplify (lst1: int list) (lst2: int list) : int list = 
	  ( if lst1 = [] then lst2 else if lst2 = [] then lst1 else
	       if lst1 = lst2 then lst1 else
		 let len1 = List.length lst1 in 
		 let len2 = List.length lst2 in 
		 let lst3 = int_list_xor lst1 lst2 in
		 let len3 = List.length lst3 in 
		 if (min len3 len2) = len3 && (min len3 len1) = len3 then lst3 
		 else if (min len1 len2) = len1 then lst1 else lst2 )
         in 
      match (simplify (m1.pieces_list) (m2.pieces_list)) with 
	| m1.pieces_list -> m1
	| m2.pieces_list -> m2
	| _ -> (self#meta_d_xor m1 m2)   *)
  
    method get_message  = Printf.printf "message:: %s \n" message; message
    method get_all_metadrops = all_metadrops

    method print_progress : unit  = 
       Printf.printf "Total pieces: %d \n" totalPieces;
       Printf.printf "message:: %s \n" message;
       Printf.printf "length of all_metadrops: %d " (List.length all_metadrops);
       ()



(*   let string_of_metadrop (m : metadrop) : string = (
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
