open Droplet
open Fountain
open Goblet



let rand_poisson (l:float) =
  let e = 2.71828183 in
  let u = Random.self_init(); Random.float 1. in
  let p = e ** (-.l) in
  let rec helper i p f =
    if u < f then i
    else let p = (l *. p) /. (float_of_int (i + 1)) in
	 let f = f +. p in
	 helper (i+1) p f
  in
  helper 0 p p

let rand_snormal () =
  Random.self_init();
  let u = Random.float 2. -. 1. in
  let v = Random.float 2. -. 1. in
  let s = (u ** 2.) +. (v ** 2.) in
  if (Random.float 1.) < 0.5
  then u *. sqrt (-.2. *. log s /. s)
  else v *. sqrt (-.2. *. log s /. s)

let rand_normal (mean:float) (var:float) =
  mean +. (sqrt var) *. rand_snormal()
