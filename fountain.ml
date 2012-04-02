open Droplet

(* the fountain produces droplets according to the fountain code 
 * implementation chosen *)
class type fountain data size =
object
  
    (* the total data (file) being transferred *)
    val mutable data

    (* the size of each piece of data from the file, eg 16 bytes *)
    val mutable piece_size

    (* total number of pieces in the original file *)
    val mutable total_pieces

    (* seed generated using a PRNG, to determine which pieces
       of the file we're using and how many pieces are in
       each XOR'd droplet *)
    val mutable seed
    
    (* for the generated droplet, how many pieces of the
       original file are we using to XOR? *)
    val mutable how_many
    
    (* this creates a new random seed to generate new droplets *)
    method random_seed

    (* this uses the seed to come up with a new random number of
       pieces being encoded in the next droplet *)
    method random_howmany

    (* this uses the seed and data variables to fetch a random
       piece of the original file *)
    method get_piece

    (* this generates a new random droplet object, using the above 
       methods and instance variables *)
    method output_droplet

end
