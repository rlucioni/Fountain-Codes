(* the Fountain, produces Droplets according to the fountain code 
 * implementation chosen *)
class type fountain =
object
    (* data to be packed up *)
    val mutable data
    
    val mutable seed
    
    (* within this chunk_size and num_chunks *)
    val mutable chunk
    
    val mutable droplet
    
    (*This updates the seed so we can decode droplets*)
    method update_seed 
end
