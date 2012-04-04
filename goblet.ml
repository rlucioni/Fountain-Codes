open Droplet

(* a Goblet, used to collect Droplets and reconstruct the original data *)
class type goblet =
object
    (* total number of pieces in the original file *)
    val mutable total_pieces
    
    (* data structure representing the part of the message we have decoded so
     * far *)
    val mutable message

    (* number that shows how much of the file we have decoded, in pieces *)
    val mutable counter

    (* this function generates a new droplet from the stream of droplets*)
    method get_droplet: unit

    (* takes a droplet as an argument, decodes if possible, updating message
     * with decoded content if applicable *)
    method decode: unit

    (* return however much we have decoded of the original message *)
    method get_message

    (* prints counter and total pieces for debugging *)
    method print_progress: unit

    (* compares counter and total_pieces, checking to see if we are done
     * decoding (i.e., counter = total_pieces) *)
    method check_complete: bool
end


class lt_goblet (d: droplet) : goblet =
object
    val mutable total_pieces = let (_,tp,_) = d in tp 
    val mutable message = 
end
