open Fountain
open Goblet
open Droplet

(** POISSON **)

class poisson_fountain (l:float) (d:string) (ps:int) (bound:int) : fountain =
object (self)
  inherit lt_fountain d ps bound as super

    method private rand_poisson =
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

    method rand_droplet_pieces =
      let pois = self#rand_poisson + 1 in
      if pois < self#get_total_pieces
      then pois else self#rand_droplet_pieces

    method get_mean = l
end

class poisson_goblet (l:float) (d:droplet) (bound:int) : goblet =
object (self)
  inherit lt_goblet d bound as super
    
    method private rand_poisson =
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

    method get_num_chunks =
      let pois = self#rand_poisson + 1 in
      if pois < self#get_total_pieces
      then pois else self#get_num_chunks

end


(** NORMAL **)

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

    method rand_droplet_pieces =
      let norm = self#rand_normal in
      if norm < (float_of_int self#get_total_pieces)
      then (int_of_float norm) else self#rand_droplet_pieces

    method get_mean = m
    method get_var = v
end

class normal_goblet (m:float) (v:float) (d:droplet) (bound:int) : goblet =
object (self)
  inherit lt_goblet d bound as super
    
    method private rand_snormal () =
      let u = Random.float 2. -. 1. in
      let v = Random.float 2. -. 1. in
      let s = (u ** 2.) +. (v ** 2.) in
      if (Random.float 1.) < 0.5
      then u *. sqrt (-.2. *. log s /. s)
      else v *. sqrt (-.2. *. log s /. s)

    method private rand_normal =
      m +. (sqrt v) *. self#rand_snormal()

    method get_num_chunks =
      let norm = self#rand_normal in
      if norm < (float_of_int self#get_total_pieces)
      then (int_of_float norm) else self#get_num_chunks

end
