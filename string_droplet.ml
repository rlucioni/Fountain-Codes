type droplet_record = {data : int list; total_pieces : int; seed : int}

(* the Droplet, containing a seed and the XOR'd data *)
class type droplet =
object
    (* a few pieces of the file encoded together *)
    val mutable data : int list
             
    (* total number of pieces in file *)
    val mutable total_pieces : int
                 
    (* pseduorandomly generated seed for telling which pieces were used *)
    val mutable seed : int
                         
    (* this converts the data, total_pieces, and seed into a string *)
    (*method to_string : string*)
                                 
    (* returns record with data, total_pieces, and seed for use in goblet *)
    method get_contents : droplet_record
end


class lt_droplet (d: int list) (t: int) (s: int) : droplet =
object
    val mutable data         = d
    val mutable total_pieces = t
    val mutable seed         = s
    
    (*
    (* still need to implement printing for testing *)
    method to_string : string =
        "{Data :" ^ data
(*(String.concat "" (List.map (string_of_int data)))*) ^ 
        ", Total pieces: " ^ (string_of_int total_pieces) ^ ", Seed: " ^
        (string_of_int seed) ^ "}" *)
                                                                    
    method get_contents = {data = data; total_pieces = total_pieces; seed = seed}
end
