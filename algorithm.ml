(* generic interface for fountain code generation *)
module type FTN_CODE =
sig
    (* kind of file being input *)
    type chunk
    
    (* encoding algorithm - can be specialized for different fountain codes *)
    val encode
    
    (* decoding algorithm - can be specialized for different fountain codes *)
    val decode
    
    (* transfer protocol - how the "fountain" is generated *)
    val gen_stream
end

(* LT structure *)
module LTCode : (FTN_CODE with type chunk = TODO) =
struct
    (* LT chunk type *)
    type chunk = TODO

    (* LT encoding function *)
    let encode = TODO

    (* LT decoding function *)
    let decode = TODO

    (* LT stream generation *)
    let gen_stream = TODO
end

(* Tornado structure *)
module TornadoCode : (FTN_CODE with type chunk = TODO) =
struct
    (* Torndao chunk type *)
    type chunk = TODO
    
    (* Tornado encoding function *)
    let encode = TODO 
    
    (* Tornado decoding function *)    
    let decode = TODO 

    (* Tornado stream generation *)
    let gen_stream = TODO
end
