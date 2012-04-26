open String_fountain ;;

class poisson_fountain (l:float) (d:string) (ps:int) (bound:int) : fountain =
object (self)
  inherit lt_fountain d ps bound as super

    method private rand_poisson (l:float) =
      let e = 2.71828183 in
      let u = Random.float 1. in
      let p = e ** (-.l) in
      let rec helper i p f =
	if u < f then i
	else let p = (l *. p) /. (float_of_int (i + 1)) in
	     let f = f +. p in
	     helper (i+1) p f
      in
      helper 0 p p

    method get_piece =
      let a = (self#rand_poisson l) in
      (Printf.printf "encoding#: %d \n" a); self#get_diced_data.(a) 
end

class normal_fountain (m:float) (v:float) (d:string) (ps:int) (bound:int) : fountain =
object (self)
  inherit lt_fountain d ps bound as super

    method private rand_snormal () =
      let u = Random.float 2. -. 1. in
      let v = Random.float 2. -. 1. in
      let s = (u ** 2.) +. (v ** 2.) in
      if (Random.float 1.) < 0.5
      then u *. sqrt (-.2. *. log s /. s)
      else v *. sqrt (-.2. *. log s /. s)

    method private rand_normal =
      m +. (sqrt v) *. self#rand_snormal() 

    method get_piece =
      let a = int_of_float self#rand_normal in
      (Printf.printf "encoding#: %d \n" a); self#get_diced_data.(a)
end

(*
let rand_snormal () =
  let u = Random.float 2. -. 1. in
  let v = Random.float 2. -. 1. in
  let s = (u ** 2.) +. (v ** 2.) in
  if (Random.float 1.) < 0.5
  then u *. sqrt (-.2. *. log s /. s)
  else v *. sqrt (-.2. *. log s /. s)

let rand_normal (mean:float) (var:float) =
  mean +. (sqrt var) *. rand_snormal()
*)