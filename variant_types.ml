type color =
  | Red
  | Blue
  | Green
  | Yellow
  | RGB of int * int * int;;

let cols = [Red; Red; Green; RGB (150, 0, 255)];;

let components c =
  match c with
  | Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b);;

type expr =
  | Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr ;;

let rec evaluate e =
  match e with
  | Num x -> x
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e';;

(* 1 + 2 ∗ 3 *)
evaluate (Add (Num 1, Multiply (Num 2, Num 3)))