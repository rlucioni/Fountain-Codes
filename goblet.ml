(* a Goblet, used to collect Droplets and reconstruct the original data *)
class type goblet =
object
    (*This function generates a new droplet from the stream of droplets*)
    method add_droplet: unit

    (*Gets the string from the droplet*)
    method get_string: 

    val mutable total_pieces

    (*Calls to diff classes*)
    val mutable droplets 
end
