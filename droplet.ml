(* the Droplet, containing a seed and the XOR'd data *)
class type droplet =
object
    (*A few chunks of the file encoded togehter*)
    val mutable data
    
    (* each individual chunk's number - inefficient*)
    val mutable how_many
    
    (* Total number of chunks in file *)
    val mutable total_pieces
    
    (* pseduorandomly generated seed for telling which chunks were used *)
    val mutable seed
    
    (*This prints the data, num_chunks, seed *)
    method to_string: unit
end
