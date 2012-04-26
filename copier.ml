if (Array.length Sys.argv) <> 3
then failwith "Usage ./copier file destination"
else ()

let file = Sys.argv.(1)
let dest = Sys.argv.(2)

let out_channel = open_out dest;;

flush out_channel ;;

let rec input_chars inchan chars =
    try
        input_chars inchan ((String.make 1 (input_char inchan)) :: chars)
    with End_of_file -> List.rev chars

let condense_chars file =
    let ch       = open_in file in
    let chars    = input_chars ch [] in
    let condense = String.concat "" chars in
        (close_in ch); condense

let big_string = condense_chars file ;;

output_string out_channel big_string ;;
flush out_channel ;;
close_out out_channel ;;
