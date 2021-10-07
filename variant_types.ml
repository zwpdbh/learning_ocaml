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



type point  = float * float;;
type shape =
  | Point  of point
  | Circle of point * float (* center and radius *)
  | Rect   of point * point (* lower-left and
                               upper-right corners *);;


type number =
  | Zero
  | Integer of int
  | Real of float ;;

let zero = Zero;;
let i = Integer 1;;
let x = Real 3.2;;

let float_of_numer = function
  | Zero -> 0.0
  | Integer i -> float_of_int i
  | Real x -> x;;

let add n1 n2 =
  match n1, n2 with
  | Zero, n
  | n, Zero ->
    n
  | Integer i1, Integer i2 ->
    Integer (i1 + i2)
  | Integer i, Real x
  | Real x, Integer i ->
    Real (x +. float_of_int i)
  | Real x1, Real x2 ->
    Real (x1 +. x2);;
add x i;;