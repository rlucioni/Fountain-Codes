open Droplet
open Random
exception TODO

(* a function for printing and debugging *)
let string_of_int_list (lst: int list) : string = 
   List.fold_right (fun x y -> (string_of_int x)^", "  ^ y ) lst ""

(* a function that removes duplicate pairs of ints an int list
 * used for xoring and list correction  *)
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

type metadrop = { number_chunks : int ; 
                  pieces_list : int list ; 
                  contents : char (*string*) }

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

    (* takes the droplet d  and returns the metadrop
     * now private  *)
    (*  method get_metadrop: droplet -> int -> metadrop *)

    (* takes a droplet runs get_metadrop and adds it to the all_metadrops *)
    method get_droplet : droplet -> unit 
    method get_droplet_list : droplet list -> unit
    (* runs on all_metadrops and trys to decode it  *)
    method decode: unit

    (* return however much we have decoded of the original message *)
    method get_message : string 

    (* just returns what we have of the message *)
    method return_message : string

    (* prints: total pieces, all_metadrops, message and counter for debugging *)
    method print_progress : unit

    (* compares counter and total_pieces, checking to see if we are done
     * decoding (i.e., counter = total_pieces) *)
    method check_complete: bool


   (* many of these will be made private later *)
  (*  method meta_d_xor: metadrop -> metadrop -> metadrop
    method metadrop_fixer: metadrop -> metadrop *)
   (* method meta_simplify: metadrop -> metadrop -> metadrop *)
    method get_all_metadrops: metadrop list
  (*  method singleKnockout: metadrop -> metadrop -> metadrop *)
    method get_all_metadrops: metadrop list
    method get_solved_singles: metadrop list
  (*  method singlesKnockout: metadrop list -> unit *)
    method remove_empties : unit
end

class lt_goblet (d: droplet) (bound: int) : goblet =
object (self)
    val mutable totalPieces = d#get_contents.total_pieces
    (* initiates the message to a string of stars of the correct length
     * for fun  *)
    val mutable message = String.make (d#get_contents.total_pieces) '_'
    val mutable all_metadrops = [] 
    val mutable solved_metadrops = []
    val mutable counter = 0
    
    (* droplet -> metadrop
     * decodes the seed information *)
    method private get_metadrop (d:droplet) (bound:int)  : metadrop = 
      let drop = d#get_contents in 
      let seed = drop.seed in 
       (*Char.escaped changes a char to a string this is for if we want to 
        * encode with string later  *) 
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
     
    (* adds a droplet to the goblet 
     *converts a droplet to a metadrops and adds it to all_metadrops *)
    method get_droplet (d: droplet) : unit = 
      let metad = (self#get_metadrop d bound) in 
     all_metadrops <- (metad::all_metadrops);
     ()
     
    method get_droplet_list (dlist: droplet list) : unit = 
      List.iter (self#get_droplet) dlist
    
    (* attempts to decode the metadrops in all_metadrops *)
    method decode : unit =
      if self#check_complete 
        then Printf.printf "Message has been fully reconstructed." 
        else 
            self#singlesKnockout solved_metadrops; 
            let rec solver (count: int): int = 
                if all_metadrops = [] 
                  then count 
                  else 
	                  let simpleM = List.fold_left (self#meta_simplify) 
                      { number_chunks = 0; 
                        pieces_list = []; 
                        contents = 'a'} all_metadrops in
                      if simpleM.number_chunks = 1 
                        then let all_metadrops_new = List.map 
                          (fun x -> self#singleKnockout simpleM x) all_metadrops in
                          all_metadrops <- all_metadrops_new; 
                          solved_metadrops <- simpleM::solved_metadrops; solver (count+1)
                        else count
            in
      let progress = solver 0 in
      if (progress) > 0 
        then(* self#remove_empties;*) let a = (counter + progress) in 
            counter <- a; Printf.printf "Message partially reconstructed. \n" 
        else Printf.printf "You must provide additional droplets. \n"
 
        
    (* removes duplicate pairs from the pieces list of a metadrop  *)
    method private metadrop_fixer (m:metadrop) : metadrop = 
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
   let num_chunk = List.length lst2 in 
    { number_chunks = num_chunk ; pieces_list = lst2 ; contents = m.contents}
   
  (* xor for metadrops *)
    method private meta_d_xor (m1:metadrop) (m2:metadrop) : metadrop  = 
      let lst = m1.pieces_list@m2.pieces_list in 
      let contents = char_of_int( (lxor) (int_of_char m1.contents) 
                                         (int_of_char m2.contents))
      in self#metadrop_fixer { number_chunks = (List.length lst); 
                               pieces_list = lst ; 
                               contents }
   
   (* xors out a singleton metadrop from m if it needs to be removed *)
    method private singleKnockout (solved_meta:metadrop) 
                                  (m:metadrop) : metadrop = 
      let knocker = solved_meta.pieces_list in 
      match knocker with 
	| hd::[] ->  let mlist = m.pieces_list in 
                  if List.exists (fun x -> x = hd) mlist 
                    then (self#meta_d_xor solved_meta m)
                     else m
        | _ -> failwith "solved_meta was not solved"


    (* removes all the solved singles from the metadrops in all_metadrops  *)
    method private singlesKnockout (solvedSingles : metadrop list) : unit = 
      let rec helper (solvedSingles: metadrop list) : unit =  
         match solvedSingles with 
	 | [] -> ()
	 | hd::tl -> let all_metadrops_new = 
                 (List.map (fun x -> self#singleKnockout hd x) all_metadrops) in
                      all_metadrops <- all_metadrops_new;
                      helper tl 
      in helper (solvedSingles)
   
   method private meta_simplify (m1:metadrop) (m2:metadrop) : metadrop =
     if m1.pieces_list = [] then m2 else if m2.pieces_list = [] then m1 else
       if m1.pieces_list = m2.pieces_list then m1 else 
         let len1 = List.length m1.pieces_list in 
         let len2 = List.length m2.pieces_list in 
         let m3 = self#meta_d_xor m1 m2 in 
         let len3 = List.length m3.pieces_list in 
        if (min len3 len2) = len3 && (min len3 len1) = len3 then m3 
		 else if (min len1 len2) = len1 then m1 else m2 
   

   method remove_empties : unit = 
     let rec helper (list : metadrop list) : metadrop list = 
        match list with
	  | [] -> []
	  | hd::[] -> if hd.number_chunks = 0 then [] else [hd]
	  | hd:: tl -> if (List.hd tl).number_chunks = 0 
                       then helper (hd::(List.tl tl)) 
                       else if hd.number_chunks = 0 
                            then helper tl 
                            else hd::(helper tl)
     in 
     let newlist = helper all_metadrops in 
     all_metadrops <- newlist 

    (* puts the solved singles into the message 
     * prints newest message *)
    method get_message: string = 
    let put (m:metadrop) : unit =  
       match m.pieces_list with
	 |[] -> raise TODO
	 |hd::[] -> message.[hd] <- m.contents
	 | hd:: tl -> raise TODO
     in
     List.iter (put) solved_metadrops;
     Printf.printf "\nKNOWN MESSAGE: %s \n" message; message

    method return_message : string = message 

    (* a way to see the other side  *)
    method get_all_metadrops = all_metadrops
    method get_solved_singles = solved_metadrops
    
    (* an early implementation of a progress printer *)
    method print_progress : unit  = 
       Printf.printf "\n \n"; 
       Printf.printf "RECONSTRUCTED MESSAGE: %s \n" message;
       Printf.printf "COUNT: %d \n" counter;
       Printf.printf "TOTAL PIECES: %d \n" totalPieces;
       Printf.printf "METADROPS CONSUMED: %d \n" 
                                                   (List.length all_metadrops); 
                                                   ()



(*   let string_of_metadrop (m : metadrop) : string = (
       "{ number_chunks :" ^ (string_of_int m.number_chunks) ^
       "; pieces_list : [" ^ (string_of_int_list m.pieces_list) ^
       "] ; contents : " ^ (m.contents) ^ " }" )
     in 
     let string_of_metadrop_list (mlst: metadrop list) : string= 
      (List.fold_right (fun x y -> ((string_of_metadrop x)^ " \n" ^ y)) mlst "")
     in 
      " totalPieces =" ^ (string_of_int totalPieces) ^ "\n 
        message = " ^ message ^ "\n
        all_metadrops = \n ^ "
        string_of_metadrop_list all_metadrops ^ " \n
        counter = " ^ string_of_int counter ^ "\n"
 *)
    method check_complete : bool = counter = totalPieces
    
end
