(* i need to develop a way to xor int lists to get out individaul ints   *) 
open Random
exception Done

(*
i should preface these comments with a TLDR tag
i wrote this out on the shuttle to get be back into it and 
clarify my thinking.

the plan: given an int list list figure out the simplest
 int list from xoring elements in the int list. empty list
 doesn't count

find a way to selectively xor out the simple int list from
 the rest of the int list list


Functions:
* lst_fixer: int list -> int list takes an int list and removes
pairs of numbers, also sorts. 
* list_lst_fixer int list list -> int list list ....
* int_list_xor int list -> int list -> int list 
 takes two int lists and xors the contents 
* simplify int list -> int list -> int list compare two int list
 and see what the shortest you can get is either by xoring or 
returning one of them
* simplify_list int list list -> int list ...
* knockout int list -> int list -> int list remove the first
 int list from the second int list by xoring if that shortens
the second int list
* knockout_through int list -> int list list -> int list 
* remove_empties  int list list -> int list list remove all empty 
lists from the int list list

working with metadrops i will have a list of singletons which are solved
pieces of data with one contributior. 

to solve a metadrop list you first list_lst_fixer then you
 remove all singletons by knockout_through

you then call simplify_list and see if you get out a metadrop thats a singleton 
meaning a metadrop that is made of only one piece

if this is the case you add the singleton to the solved pieces and add its data to the message
then recursively decode the int list list to see if you can get out another singleton
if this is not the case you raise exception that you need more droplets to make progress 

Note: have a counter that keeps track of the solutions found in a call of solver, if no singleton and
no solutions then return the exception need more droplets 
if it gets one singleton but doesnt finish then return the singletons 


*)


 (* takes an int list and removes ints if they appear an even number of time
  * leaves one if they appear an odd number of times *)
 let lst_fixer (lst:int list) : int list =
 let lstSorted = List.sort compare lst in 
  let rec duplicate_remover (lst: int list) : int list = 
    ( match lst with 
      |[] -> []
      |hd::[] -> [hd]
      |hd::tl -> if hd = List.hd tl then duplicate_remover (List.tl tl)
         else hd::duplicate_remover tl)
       in  duplicate_remover lstSorted
;;
 let _ = assert (lst_fixer [1;1] = [] )
 let _ = assert (lst_fixer [1;1;1] = [1])
 let _ = assert (lst_fixer [1;2;3;4;4;5;1] = [2;3;5])

 (* maps list_fixer to an in list list *)
 let list_lst_fixer (lstlst: int list list) : int list list = 
  List.map lst_fixer lstlst

 let _ = assert ( list_lst_fixer [[1;2;3;4];[1;1;1];[4;3;4]] = [[1;2;3;4];[1];[3]] )

(* fixing two int list the same as xoring them  *) 
let int_list_xor (lst1: int list) (lst2: int list) : int list =
  let lst = lst1@lst2 in 
   lst_fixer lst

let _ = assert (int_list_xor [1;2;3;4] [2;4] = [1;3])
let _ = assert (int_list_xor [1;3;5;7] [2;4;5;6] = [1;2;3;4;6;7])

(* given two int lists return the most simplified between 
 * lst1, lst2 and the xor of the two *) 
let simplify (lst1: int list) (lst2: int list) : int list = 
  if lst1 = [] then lst2 else if lst2 = [] then lst1 else
  if lst1 = lst2 then lst1 else
  let len1 = List.length lst1 in 
  let len2 = List.length lst2 in 
  let lst3 = int_list_xor lst1 lst2 in
  let len3 = List.length lst3 in 
  if (min len3 len2) = len3 && (min len3 len1) = len3 then lst3 
  else if (min len1 len2) = len1 then lst1 else lst2 
  
let _ = assert (simplify [1] [1;2;3;4] = [1])
let _ = assert (simplify [1;2;3] [1;2;3;4] = [4] )
let _ = assert (simplify [1;2;3;4;5] [2;3;7;5] = [1;4;7])
let _ = assert (simplify [] [1;5;6] = [1;5;6])

let simplify_list (lst: int list list) : int list = 
   List.fold_left simplify [] lst 

let _ = assert (simplify_list [[1;2;3];[4];[];[3;2;6]] = [4])
(* Note: simplify is not perfect because it assumes combinations will
 * be either next to each other or made with one of the shortest int list
 * when in reality a length 6 and 7 list could make a length 1 but we carry 
 * through the inital length three because the 6 and 7 aren't sequential  *)
let _ = assert (simplify_list [[1;2;3];[3;5;4;7;8];[1;2;3;4]] = [4])

 (* knockout should xor the knocker with the lst if it has something
  * to remove from the lst leave it unchanged otherwise  *)
 let knockout (knocker:int list) (lst: int list) : int list = 
     if knocker = [] then lst else 
       match knocker with 
	 |[a] -> List.filter (fun x -> x != a) lst
	 | _ -> simplify knocker lst

 let _ = assert (knockout [4] [1;2;3;4] = [1;2;3])
 let _ = assert (knockout [4] [1;2;3;5] = [1;2;3;5])
 let _ = assert (knockout [5;6;1] [1;3;5;4] = [3;4;6])
(* this last assert demonstrates the flaw of knockout, i didnt really
 * implement knockout in full it only really works for the case where the 
 * knocker is an int list of length 1. *) 
let _ = assert (knockout [2;3] [1;2;4] = [2;3])

(*
  * removes an int list from all the lists *)
 let knockout_through (remover: int list) (lst: int list list) : int list list = 
  List.map (knockout remover) lst

 let _ = assert (knockout_through [1] [[1;2;3];[2;3;5];[5;43;1]] = [[2;3];[2;3;5];[5;43]])
(* because of the noted flaw in knockout this only really works well when the
 * remover is a list of length 1 *)

(* remove the empties int list from an int list list *) 
let rec remove_empties (lstlst: int list list) : int list list =
     match lstlst with
       |[] -> []
       | hd::[] -> [hd]
       | hd::tl ->  if List.hd tl = []
                    then remove_empties (hd::(List.tl tl))
                    else if hd = [] 
                         then remove_empties tl
                         else hd::remove_empties tl 
let _ = assert (remove_empties [[];[1;2;3];[];[];[];[4;5;6;7];[];[]] = [[1;2;3];[4;5;6;7]])


(* :::Testing Helpers:::
 * bound = max value 
 * lstLen = number of int lists in the list
 * maxlen = max length of int list in the list 
*)

(* generates a random int list of length with bound
 * Note: assumes init has been seeded and random is open  *)
 let rec list_gen (length: int) (b: int) : int list = 
    if length <= 0 then [] else (int b)::(list_gen (length-1) b) 

let int_list_gen (seed: int) (bound: int) (lstLen:int) (maxlen:int) : int list list = 
  init seed;
  let rec listL_gen (bound:int) (number:int) (maxlen:int) : int list list = 
 if number <= 0 then [] else let length = int maxlen in 
   (list_gen length bound)::(listL_gen bound (number-1) maxlen) 
  in 
  (listL_gen bound lstLen maxlen)


(*  So you have an int list list

call list_lst_fixer to get rid of duplicates 
call remove_empties to ...
call simplify_list to get the simpliest int list out
call knockout_through with that simple list and the int list
call list_lst_fixer for good measure 
call remove_empties 

call simplify_list to ...
call knockout_through ...
call list_lst_fixer...

...

*)
let int_list_to_string (lst: int list) : string = 
  let rec intsHelper (lst:int list) : string = 
    ( match lst with 
       | [] -> ""
       | hd::tl -> (string_of_int hd)^", "^(intsHelper tl))
  in "["^ (intsHelper lst) ^"]"
;;
let rec ill_to_string (lstlst: int list list) : string = 
   match lstlst with
     | [] -> ""
     | hd::tl -> int_list_to_string hd ^ ill_to_string tl
;;      

let actor (lstlst: int list list ) : (int list * int list list) = 
   if lstlst = [] then (Printf.printf "all Done"; raise Done) else  
let a = list_lst_fixer lstlst in 
  let a' = remove_empties a in 
  let a_knocker = simplify_list a' in
  let b = knockout_through a_knocker a' in 
  let b' = list_lst_fixer b in
  let b'' = remove_empties b' in 
  (a_knocker, b'')


(*
 *
 *)
let rec SinglesKnockout (solvedSingles: int list list) ( lst_lst: int list list) : int list list = 
  match solvedSingles with
    |[] -> lstlst
    | hd::tl -> let lst = knockout_through hd lst_lst in SinglesKnockout tl lst 

exception NeedMoreInfo
 
(* should return a list of solved singletons
 should return left over unsolved pieces
 should return number of new singletons   *)
let solverB (solvedSingles: int list list) (lst_lst: int list list) : (int list list* int list list * int) =
  let list2 = SinglesKnockout solvedSingles lst_lst in 
   let rec solver2 (solvedSingles:int list list) (lstlst: int list list ) (count:int) : (int list list * int list list * int) = 
  ( if lstlst = [] then (Printf.printf "all Done"); (solvedSingles,[],count) else  
    let a' = remove_empties (list_lst_fixer lstlst) in  
    let a_knocker = simplify_list a' in
   if List.length a_knocker = 1 then 
    let b = knockout_through a_knocker a' in 
    let b'' = remove_empties  (list_lst_fixer b) in
    in solver2 (a_knocker::solvedSingles) (b'') (count +1) 
 else (solvedSingles,a',count)  )

in solver2 solvedSingles list2 0


let int_of_singleton (lst:int list): int = 
   match lst with
     | [] -> -1
     | [a]-> a
     | [_] -> -1

let rec stepper (repeat:int) (tup: int list * int list list) : (int list * int list list) = 
  if repeat <=0 then tup else  
let (lst,lstlst) = tup in 
  Printf.printf "knocked out! %d \n " (int_of_singleton lst); 
  stepper (repeat-1) (actor lstlst)

let rec solver (lstlst:int list list ) = 
  let a = actor lstlst in  
let b = stepper a in
let rec loop (tup: int list*int list list ) = 
  let (lst,lstlst) = tup in 
   if lst = [] then Printf.printf "solved it! \n " ; ([],[[]])
	 else Printf.printf "working"; let a = stepper tup in 
      loop a 
loop b


let list = [[1;2];[3;4;5];[1;2;5];[4;1;2];[2;3;5]]


   
