(*list*)

(*pattern matching*)
let rec sum lst =
  match lst with
  | [] -> 0
  | h::t -> h + sum t;;
(* sum [1; 2; 3];; *)

    (* _::[] matches all lists with exactly one element
     * 
     * _::_ matches all lists with at least one element
     * 
     * _::_::[] matches all lists with exactly two elements
     * 
     * _::_::_::_ matches all lists with at least three elements *)



(*tail recursion*)
let rec sum (l : int list) : int =
  match l with
    [] -> 0
  | x :: xs -> x + (sum xs);;

let rec sum_plus_acc (acc : int) (l : int list) : int =
  match l with
    [] -> acc
  | x :: xs -> sum_plus_acc (acc + x) xs;;

let sum_tr : int list -> int = 
  sum_plus_acc 0;;


(*Variant, like enum*)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat ;;

let int_of_day d =
  match d with
  | Sun -> 1 
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7;;
(*# int_of_day Sun ;;*)






(*3.1.4 Record, like struct in C.*)
type ptype = 
  TNormal | TFire | TWater

type peff = 
  ENormal | ENotVery | ESuper

(*ptype here is used as both a type name and a field name*)
type mon = {name: string; hp : int; ptype: ptype};;




(*3.1.5 Tuples*)
(*Similar as Record. But it is access by position instead of by field name.*)

match (1,2,3) with
| (x,y,z) -> x+y+z;;

(*********************************************
 * Several ways to get a Pokemon's hit points:
 *********************************************)
(* OK *)
let get_hp m =
  match m with
  | {name=n; hp=h; ptype=t} -> h

(* better *)
let get_hp m =
  match m with
  | {name=_; hp=h; ptype=_} -> h

(* better *)
let get_hp m =
  match m with
  | {name; hp; ptype} -> hp

(* better *)
let get_hp m =
  match m with
  | {hp} -> hp


(* best, just access by its field name *)
let get_hp m = m.hp


(**************************************************
 * Several ways to get the 3rd component of a tuple
 **************************************************)
(* OK *)
let thrd t =
  match t with
  | (x, y, z) -> z

(* good *)
let thrd t = 
  let (x,y,z) = t in z

(* better *)
let thrd t =
  let (_,_,z) = t in z

(* best *)
let thrd (_,_,z) = z;;






(*3.2.1 Options*)
let extract o =
  match o with
  | Some i -> string_of_int i
  | None -> "";;
extract (Some 42);;
extract None;;