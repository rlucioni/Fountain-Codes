module type FTN_CODE =
sig
    (* kind of file being input *)
    type file
    
    (* encoding algorithm - can be specialized for different fountain codes *)
    val encode
    
    (* decoding algorithm - can be specialized for different fountain codes *)
    val decode
    
    (* transfer protocol - how the "fountain" is generated *)
    val gen_stream
end
