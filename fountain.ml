open Random;;
open Droplet;;

(* the fountain produces droplets according to the fountain code 
 * implementation chosen *)

class type fountain =
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
    
    (* for the generated droplet, how many pieces of the
       original file are we using to XOR? *)
    val mutable how_many : int
    
    (* this creates a new random seed to generate new droplets *)
    method random_seed : unit

    (* this uses the seed to come up with a new random number of
       pieces being encoded in the next droplet *)
    method random_howmany : unit

    (* this uses the seed and data variables to fetch a random
       piece of the original file *)
    method get_piece : int

    method xor : int

    (* this generates a new random droplet object, using the above 
       methods and instance variables *)
    method output_droplet : droplet
end


class lt_fountain (d: string) (ps: int) (bound : int) : fountain =
object (this)
    val mutable data         = d
    val mutable piece_size   = ps
    val mutable total_pieces = (String.length d) / ps
    val mutable seed         = bits ()
    val mutable how_many     = 0

    method random_seed       = seed <- bits (); init seed
    
    method random_howmany    = how_many <- int bound
    
    method get_piece         = int_of_char data.[int total_pieces]

    method xor               = this#random_seed; this#random_howmany;
        let rec help_xor (n:int) : int =
	        if (n > 0) 
                then ((lxor) (this#get_piece) (help_xor n-1))
	            else this#get_piece
        in
        help_xor how_many

    method output_droplet    = this#random_seed; this#random_howmany;
                               new lt_droplet (char_of_int this#xor) 
                                             (total_pieces)
                                             (seed)
end
