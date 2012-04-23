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
