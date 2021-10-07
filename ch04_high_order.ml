let double x = x + x;;
let square x = x * x;;

(*apply*)
let apply f x = f x;;

(*pipeline*)
let pipeline x f = f x;;
let (|>) = pipeline;;
let x = 5 |> double;;

(*Compose*)
let compose f g x = f (g x);;

let square_then_double = compose double square;;
square_then_double 2;;


(*Both: A function that applies two function and return a pair of the result*)
let both f g x = (f x, g x);;
let ds = both double square;;
ds 3;;


(*Cond: a function that conditionally choose which of two functions to apply based on a predicate*)
let cond p f g x =
  if p x then f x else g x ;;


(*map transforms elements;
  filter eliminates elements;
  fold combines elements*)


(*Map*)
let rec add1 = function
  | [] -> []
  | h::t -> (h+1)::(add1 t);;
add1 [1; 2; 3];;

let rec concat3110 = function
  | [] -> []
  | h::t -> (h^"3110")::(concat3110 t);;
concat3110 ["a"; "b"; "c"];;

let rec map f l =
  match l with
  | [] -> []
  | h::t -> (f h)::(map f t);;
let add1 = map (fun x -> x + 1);;



(*filter*)
let rec filter f l =
  match l with
  | [] -> []
  | h::t -> if f h then h::(filter f t) else filter f t;;


(*fold*)
let rec combine op init l =
  match l with
  | [] -> init
  | h::t -> op h (combine op init t);;
let sum    = combine (+) 0;;

(*Remember:  First note that (most) any infix operator in OCaml can also be used as a prefix operator by surrounding the operator with parens and putting it in a prefix position, like so: 2 + 3 = (+) 2 3.*)
let sum = combine (+) 0;;
let concat = combine (^) "";;

(*The reason it is called fold_right is that it folds in element of the list from right to left.*)
let rec fold_right op lst init =
  match lst with
  | []   -> init
  | h::t -> op h (fold_right op t init);;

let sum lst = fold_right (+) lst 0;;

(*Fold_left*)
let rec fold_left op acc lst =
  match lst with
  | []     -> acc
  | h :: t -> fold_left op (op acc h) t;;

(*Notice its differences with fold_right, consider the following example of "folding from left to right*)
  let rec sum' acc = function
  | []   -> acc
  | h :: t -> sum' (acc + h) t;;

  (*Rewrite sum' using fold_left*)
  let sum = List.fold_left (+) 0;;



  let rec fold_left f accu l =
  match l with
  |  [] -> accu
  | a::l -> fold_left f (f accu a) l;;

  let rec fold_right f l accu =
  match l with
  |  [] -> accu
  | a::l -> f a (fold_right f l accu);;

  let (--) i j =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j [];;


  let fold_right_tr f l accu =
  List.fold_left (fun acc elt -> f elt acc) accu (List.rev l);;
  fold_right_tr (fun x y -> x - y) (0--1_000_000) 0;;



  (*a way of understanding fold_right*)
  type 'a list =
  | Nil
  | Cons of 'a * 'a list;;

  let rec foldlist init op l =
  match l with
  | Nil -> init
  | Cons (h, t) -> op h (foldlist init op t);;


  (*Define fold for tree*)
  type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;

  let rec foldtree init op tr =
  match tr with
  | Leaf -> init
  | Node (v, l, r) -> op v (foldtree init op l) (foldtree init op r);;

  let size t = foldtree 0 (fun _ l r -> 1 + l + r) t;;
  let depth t = foldtree 0 (fun _ l r -> 1 + max l r) t;;
  let preorder t = foldtree [] (fun x l r -> [x] @ l @ r) t;;
  (*This technique constructs something called a catamorphism, aka a generalized fold operation.*)

  List.fold
