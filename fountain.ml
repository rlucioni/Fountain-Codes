open Random;;
open Droplet;;

(* the fountain produces droplets according to the fountain code 
 * implementation chosen *)

class
 type fountain =
object
  
    (* the total data (file) being transferred *)
    val mutable data : string

    (* the size of each piece of data from the file, eg 16 bytes *)
    val mutable piece_size : int

    (* total number of pieces in the original file *)
    val mutable total_pieces : int

    (* seed generated using a PRNG, to determine which pieces
       of the file we're using and how many pieces are in
       each XOR'd droplet *)
    val mutable seed : int
    
    (* number of pieces of the original file to be XOR'd in the new droplet *)
    val mutable droplet_pieces : int
    
    (* this creates a new random seed to generate new droplets *)
    method random_seed : unit

    (* this uses the seed to come up with a new random number of
       pieces being encoded in the next droplet *)
    method rand_droplet_pieces : unit

    (* this uses the seed and data variables to fetch a random
       piece of the original file *)
    method get_piece : int

    method private xor : int

    (* this generates a new random droplet object, using the above 
       methods and instance variables *)
    method output_droplet : droplet
end


class lt_fountain (d: string) (ps: int) (bound : int) : fountain =
object (this)
  (*   type droplet = lt_droplet
   *  we may need some sort of a line like this ! *) 
    val mutable data           = d
    val mutable piece_size     = ps
    val mutable total_pieces   = (String.length d) / ps
    val mutable seed           = bits ()
    val mutable droplet_pieces = 0

    method random_seed         = seed <- bits (); init seed
    
    method rand_droplet_pieces = droplet_pieces <- (int bound) + 1
    
    (* we may be able to abstract this out to create different distributions *)
    method get_piece           = (* int_of_char data.[int total_pieces]*) 
      let a = (int total_pieces) in (Printf.printf "encoding#: %d \n" a); int_of_char data.[a]                             


    (* not a public method *)
    method private xor         =
        let rec help_xor (n:int) : int =
	        if (n > 1) 
                then ((lxor) (this#get_piece) (help_xor (n-1)))
	            else this#get_piece
        in
        help_xor droplet_pieces

    method output_droplet    = this#random_seed; this#rand_droplet_pieces;
                               new lt_droplet (char_of_int this#xor) 
                                              (total_pieces)
                                              (seed)
end


let _ = c#get_droplet (a#output_droplet);;


