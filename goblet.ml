open Droplet
open Random
exception TODO

(* a function for printing and debugging *)
let string_of_int_list (lst: int list) : string = 
    List.fold_right (fun x y -> (string_of_int x)^", "  ^ y ) lst ""

(* a function that removes duplicate pairs of ints in an int list
 * used for xoring and list correction  *)
let lst_fixer (lst:int list) : int list =
    let lstSorted = List.sort compare lst in 
    let rec duplicate_remover (lst: int list) : int list = 
        (match lst with 
         | [] -> []
         | hd::[] -> [hd]
         | hd::tl -> 
             if hd = List.hd tl 
                 then duplicate_remover (List.tl tl)
                 else hd::duplicate_remover tl)
    in  
    duplicate_remover lstSorted

let int_list_xor (lst1: int list) (lst2: int list) : int list =
    let lst = lst1@lst2 in 
    lst_fixer lst

type metadrop = {number_chunks : int; 
                 pieces_list   : int list; 
                 contents      : int list}


(* NOTE: A "metadrop" consists of the metadata associated with a particular
 * droplet. This metadata is extracted decoding the seed information in the
 * droplet *)

(* a Goblet, used to collect Droplets and reconstruct the original data *)
class type goblet =
object
    (* total number of pieces in the original file *)
    val mutable totalPieces : int
    
    (* total number of metadrops the goblet has consumed *)
    val mutable metadrops_consumed : int
 
    (* a list of all metadrops in the goblet  *)
    val mutable all_metadrops : metadrop list
    
    (* a list of metadrops that are made of one chunk *) 
    val mutable solved_metadrops : metadrop list
    
    (* a string representing the part of the message we have decoded so far *)
    val mutable message : string 
    
    (* a list of metadrops that are made of one chunk and not yet added to 
     * message *)
    val mutable to_add_message : metadrop list
    
    (* a count of how many droplets have been dropped*)
    val mutable drop_count : int

    (* number that shows how much of the file we have decoded, in pieces *)
    val mutable counter : int

    (* uses the PRNG to generate the values used in each droplet *)
    method get_num_chunks : int

    (* takes one droplet, decodes the seed information, and adds it to the pool
     * of metadrops *)
    method get_droplet : droplet option -> unit 

    (* takes more than one droplet, runs their seed information, and adds them
     * to the pool of metadrops*)
    method get_droplet_list : droplet option list -> unit
    
    (* takes all_metadrops and trys to decode them *)
    method decode : unit

    (* return however much we have decoded of the original message *)
    method get_message : string 

    (* prints total pieces, all_metadrops, message and counter for debugging *)
    method print_progress : unit

    (* returns how many metadrops have been used so far *)
    method num_used : int

    (* compares counter and total_pieces, checking to see if we are done
     * decoding (i.e., counter = total_pieces) *)
    method check_complete: bool

    method get_all_metadrops: metadrop list
    
    method get_solved_singles: metadrop list
    
    (* removes empty metadrops from the metadrop pool *)
    method remove_empties : unit
    
    (* returns the totalPieces instance variable *)
    method get_total_pieces : int
end

class lt_goblet (d: droplet) (bound: int) : goblet =
object (self)
    val mutable totalPieces = d#get_contents.total_pieces
    
    (* prints a string of the same length as the message, consisting of
     * underscores; intended to help visualize the message's reconstruction 
     * when printing is enabled *)
    val mutable message = 
        String.make (d#get_contents.total_pieces * 
                                          (List.length d#get_contents.data)) '*'
    val mutable all_metadrops = []

    val mutable solved_metadrops = []
    
    val mutable metadrops_consumed = 0   
 
    val mutable counter = 0
     
    val mutable drop_count = 0    

    val mutable to_add_message = []
    
    val mutable piece_size = List.length d#get_contents.data
    
    val mutable extra = d#get_contents.extra

    (* decodes the seed information *)
    method private get_metadrop (d:droplet) : metadrop = 
        let drop     = d#get_contents in 
        let seed     = drop.seed      in 
        let contents = (drop.data)    in
        init seed; 
        let num_chunks = self#get_num_chunks in 
	    let rec get_int_list (n:int) : int list = 
            (if n > 1  
                then int totalPieces :: get_int_list (n-1)
                else [(int totalPieces)] )
        in
        self#metadrop_fixer {number_chunks = num_chunks; 
                             pieces_list   = (get_int_list num_chunks);
                             contents}

    method private get_num_chunks = (int bound) + 1
    
    (* adds a droplet to the goblet; converts a droplet to a metadrops and adds
     * it to all_metadrops, the metadrop pool *)
    method get_droplet (drop: droplet option) : unit = 
        match drop with 
	  | None -> (drop_count <- 1 + drop_count) 
	  | Some (d) -> 
        let metad = (self#get_metadrop d) in 
        all_metadrops <- (metad::all_metadrops);
        metadrops_consumed <- metadrops_consumed + 1;
         ()
     
    method get_droplet_list (dlist: droplet option list) : unit = 
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
                    {number_chunks = 0; 
                     pieces_list   = []; 
                     contents      = []} all_metadrops 
                  in
                  if simpleM.number_chunks = 1 
                    then 
                      let all_metadrops_new = List.map 
                          (fun x -> self#singleKnockout simpleM x) 
                          all_metadrops
                      in
                      all_metadrops <- all_metadrops_new; 
                      solved_metadrops <- simpleM::solved_metadrops; 
                      to_add_message <- simpleM::to_add_message;
                      solver (count + 1)
                  else (self#remove_empties; count)
            in
            let progress = solver 0 in
            if (progress) > 0 
              then
                let a = (counter + progress) in counter <- a 
                (* ;Printf.printf "Message partially reconstructed. \n" *) 
              else 
                (* Printf.printf "You must provide additional droplets. \n"; *) 
                ()
        
    (* removes duplicate pairs from the pieces list of a metadrop *)
    method private metadrop_fixer (m:metadrop) : metadrop = 
        let lst_fixer (lst:int list) : int list =
            (let lstSorted = List.sort compare lst in 
            let rec duplicate_remover (lst: int list) : int list = 
                (match lst with 
                | []       -> []
                | hd :: [] -> [hd]
                | hd::tl   -> 
                    if hd = List.hd tl 
                      then duplicate_remover (List.tl tl)
                      else hd::duplicate_remover tl)
            in  duplicate_remover lstSorted)
        in 
        let lst = m.pieces_list in 
        let lst2 = lst_fixer lst in
        let num_chunk = List.length lst2 in 
        {number_chunks = num_chunk; pieces_list = lst2; contents = m.contents}
   
    (* XOR for metadrops *)
    method private meta_d_xor (m1:metadrop) (m2:metadrop) : metadrop  = 
      let lst      = m1.pieces_list@m2.pieces_list in 
      let contents = List.map2 (lxor) m1.contents m2.contents in 
      self#metadrop_fixer {number_chunks = (List.length lst); 
                           pieces_list = lst; 
                           contents}

    (* XORs out a singleton metadrop from a metadrop if the metadrop contains 
     * the singleton in question *)
    method private singleKnockout (solved_meta:metadrop) 
                                                      (m:metadrop) : metadrop = 
      let knocker = solved_meta.pieces_list in 
      match knocker with 
	  | hd::[] ->  
          let mlist = m.pieces_list in 
          if List.exists (fun x -> x = hd) mlist 
            then (self#meta_d_xor solved_meta m)
            else m
      | _      -> failwith "Contract breach: solved_meta was not solved"


    (* removes all the solved singles from the metadrops in all_metadrops *)
    method private singlesKnockout (solvedSingles : metadrop list) : unit = 
      let rec helper (solvedSingles: metadrop list) : unit =  
        match solvedSingles with 
	    | []     -> ()
	    | hd::tl -> 
            let all_metadrops_new = 
                 (List.map (fun x -> self#singleKnockout hd x) all_metadrops) 
            in
            all_metadrops <- all_metadrops_new;
            helper tl 
      in helper (solvedSingles)
   

    method private meta_simplify (m1:metadrop) (m2:metadrop) : metadrop =
      if m1.pieces_list = [] 
        then m2 
        else 
          if m2.pieces_list = [] 
            then m1 
            else
             if m1.pieces_list = m2.pieces_list 
               then m1 
               else 
                 let len1 = List.length m1.pieces_list in 
                 let len2 = List.length m2.pieces_list in 
                 let m3   = self#meta_d_xor m1 m2      in 
                 let len3 = List.length m3.pieces_list in 
                 if (min len3 len2) = len3 && (min len3 len1) = len3 
                   then m3 
		           else 
                     if (min len1 len2) = len1 
                       then m1 
                       else m2 
   

    method remove_empties : unit = 
      let rec helper (list : metadrop list) : metadrop list = 
        match list with
	    | []      -> []
	    | hd::[]  -> 
            if hd.number_chunks = 0 
              then [] 
              else [hd]
	    | hd:: tl -> 
            if (List.hd tl).number_chunks = 0 
              then helper (hd::(List.tl tl)) 
              else 
                if hd.number_chunks = 0 
                  then helper tl 
                  else hd::(helper tl)
      in 
      let newlist = helper all_metadrops in 
      all_metadrops <- newlist 

    (* puts the solved singles into the message; prints newest message *)
    method get_message: string =
      let rec string_int (lst:int list) =
        match lst with
        | []     -> ""
        | hd::tl -> (String.make 1 (char_of_int hd)) ^ (string_int tl)
      in
      let put (m:metadrop) : unit =  
         match m.pieces_list with
	     | hd::[] -> 
               String.blit (string_int m.contents) 0 
                           message (hd*piece_size) piece_size
	     | _      -> failwith "Impossible result in get_message."
      in
      List.iter put to_add_message (*solved_metadrops*);
      (*Printf.printf "\033[KKNOWN MESSAGE: %s" message;*)
      to_add_message <- []; 
      let length = totalPieces * piece_size in
      let length' = length - extra          in
      String.sub message 0 length'
    
    (* methods which allow access to instance variables *)
    method get_all_metadrops = all_metadrops
    method get_solved_singles = solved_metadrops
    
    (* prints statistics and progress as we reconstruct the message *)
    method print_progress : unit  = 
       (*Printf.printf "\n \n"; 
       Printf.printf "RECONSTRUCTED MESSAGE: %s \n" message;
       Printf.printf "COUNT: %d \n" counter; *)
       Printf.printf "   IDEAL PACKET CONSUMPTION: %d   " totalPieces;
       Printf.printf "\rPACKETS CONSUMED: %d    PACKETS DROPPED: %d  " 
                                             (metadrops_consumed) (drop_count);
       flush_all ()

    method num_used : int = metadrops_consumed (*List.length all_metadrops*)

    method check_complete : bool = counter = totalPieces

    method get_total_pieces = totalPieces
end
