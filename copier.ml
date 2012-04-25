let in_channel  = open_in  "lenna.jpg";;
let out_channel = open_out "image_dump.jpg";;

flush out_channel;;

let rec copier () =
    (*let line = input_line in_channel in*)
    output_string out_channel (input_line in_channel); output_char out_channel
    '\n'; copier ()
in
copier ()
