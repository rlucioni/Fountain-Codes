(* generic interface for fountain code generation *)
module type FTN_CODE =
sig
    exception TODO

    (* kind of file being input *)
    type packet
    
    (* defining *)
    type stream

    (* seeds packet_confirmation below with universal value for easy testing *)
    val seed
    
    (* simulates lossy connection by checking if a packet was received or not *)
    val rcvd : unit -> bool
    
    (* encoding algorithm; opens file and returns packets in list form *)
    val list_encode : string -> stream
    
    (* decoding algorithm; takes list as argument and returns a tuple 
       containing the decoded file in list form, along with an int showing
       difference between total packets in file and packets used to decode *)
    val list_decode : list -> (stream * int)
    
    (* encoding algorithm that opens a file and streams packets into 
       directory *)
    val file_encode : string -> string -> unit
    
    (* decodes packets in a directory, creates a second file, and returns 
       difference between total packets in file and packets used to decode *)
    val file_decode : string -> string -> int
    
end

(* LT structure *)
module LTCode : (FTN_CODE with type chunk = raise TODO) =
struct
    (* LT chunk type *)
    type chunk = raise TODO

    (* LT encoding function *)
    let encode = raise TODO

    (* LT decoding function *)
    let decode = raise TODO

    (* LT stream generation *)
    let gen_stream = raise TODO
end

(* Tornado structure *)
module TornadoCode : (FTN_CODE with type chunk = raise TODO) =
struct
    (* Torndao chunk type *)
    type chunk = raise TODO
    
    (* Tornado encoding function *)
    let encode = raise TODO 
    
    (* Tornado decoding function *)    
    let decode = raise TODO 

    (* Tornado stream generation *)
    let gen_stream = raise TODO
end
