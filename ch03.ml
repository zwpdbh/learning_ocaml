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


(*use Option to define list_max*)
let rec list_max = function
  | [] -> None
  | h::t -> begin
      match list_max t with
      | None -> Some h
      | Some m -> Some (max h m)
    end;;

(*3.2.2 Association Lists*)
let d = [("rectangle", 4); ("triangle", 3); ("dodecagon", 12)];;

let insert k v d = (k, v)::d ;;
let rec lookup k d =
  match d with
  | [] -> None
  | (k', v)::t -> if k=k' then Some v else lookup k t;;





(*3.2.3 Type Synonyms*)
type point = float * float ;;
type vector = float list ;;
type matrix = float list list;;

(*3.2.4 Algebraic Data Type*)

type shape =
  | Point of point
  | Circle of point * float
  | Rect of point * point;;


let pi = 3.1415926;;
let area shape =
  match shape with
  | Point _ -> 0.0
  | Circle (_, r) -> pi *. (r ** 2.0)
  | Rect ((x1, y1), (x2, y2)) ->
     let w = x2 -. x1 in
     let h = y2 -. y1 in
     w *. h;;

let center = function
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2)) ->
     ((x2 +. x1 /. 2.0),
      (y2 +. y1) /. 2.0);;

(*Using variants, we can express a type that represents the union of several other types.*)

type string_or_int =
  | String of string
  | Int of int;;
type string_or_int_list = string_or_int list;;

let rec sum: string_or_int_list -> int = function
  | [] -> 0
  | (String s)::t -> int_of_string s + sum t
  | (Int i)::t -> i + sum t;;
let three = sum [String "1"; Int 2];;

(*3.2.4.2 Recursive Variants*)
type intlist = Nil | Cons of int * intlist;;

let lst3 = Cons (3, Nil);;
let lst123 = Cons (1, Cons (2, lst3));;

let rec sum (l:intlist) : int =
  match l with
  | Nil -> 0
  | Cons (h, t) -> h + sum t;;

let rec length : intlist -> int = fun l ->
  match l with
  | Nil -> 0
  | Cons (_,t) -> 1 + length t;;

let rec length01 l =
  match l with
  | Nil -> 0
  | Cons (_,t) -> 1 + length l;;

let rec length02 (l:intlist) : int =
  match l with
  | Nil -> 0
  | Cons (_,t) -> 1 + length l;;

let empty : intlist -> bool = function
  | Nil -> true
  | Cons _ -> false;;

(*3.2.4.3 Parameterized Variant*)
type 'a mylist = Nil | Cons of 'a * 'a mylist;;


(*3.2.4.4 Polymorphic Variants*)
let f = function
  | 0 -> `Infinity
  | 1 -> `Finite 1
  | n -> `Finite (-n);;

match f 3 with
| `NegInfinity -> "negative infinity"
| `Finite n -> "finite"
| `Infinity -> "infinite";;