(*list expression*)
let lst = [1; 2; 3; 4; 5];;

let rec drop_value l x =
  match l with
  | [] -> []
  | h :: t ->
     let new_l = drop_value t x in
     if h = x then new_l else h :: new_l;;

let build_list m n =
  let (x, y) =
    if m > n then (n, m) else (m, n) in
  let rec aux cpt acc =
    match cpt with
    | n when n < x -> acc
    | _ -> aux (cpt -1) (cpt::acc)
  in aux y [];;

(*use when to match a range of a variable*)
let my_range x =
  match x with
  | n when n > 0 -> build_list 0 n
  | _ -> [];;

let rec my_product l =
  match l with
  | [] -> 1
  | h :: t -> h * my_product t;;

my_product (build_list 1 3);;

let rec my_concat_str_from_list l =
  match l with
  | [] -> ""
  | h :: t -> h ^ my_concat_str_from_list t;;
my_concat_str_from_list ["a"; "b"; "abcd"];;

let is_bigred l =
  match l with
  | h :: t when h = "bigred" -> true
  | _ -> false;;

let two_or_four l =
  match l with
  | _::_::[] -> true
  | _::_::_::_::[] -> true
  | _ -> false ;;

let first_two_equal l =
  match l with
  | x1::x2::t when x1 = x2 -> true
  | _ -> false ;;

let get_fifth l =
  match List.length l with
  | x when x >= 5 -> List.nth l 4
  | _ -> 0;;

let sorted_list l =
  match l with
  | [] -> []
  | _ -> let list_rev = List.sort (fun x y -> if x < y then 1 else 0 ) l in List.rev list_rev;;

let last_of_list l =
  let n = List.length l in
  List.nth l (n-1);;

let any_zeros l =
  List.for_all (fun x -> x = 0) l ;;
any_zeros [1;2;3;0];;



let take n l =
  let rec take_aux  rest c acc =
    match rest with
    | [] -> acc
    | h::t when c < n -> take_aux t (c+1) (h::acc)
    | _::_  -> acc in
  let rev = take_aux l 0 [] in
  List.rev rev;;

take 2 [1;2;3;4;5];;
take 2 [1];;

let drop n l =
  let rec drop_aux n rest c =
    match rest with
    | [] -> []
    | h::t when c < n -> drop_aux n t (c+1)
    | _::t -> t
  in drop_aux (n-1) l 0;;
drop 2 [1;2;3;4;5;6;7;8];;
drop 2 [1];;
drop 2 (my_range 10000000);;


let rec keep_increasing l =
  match l with
  | [] -> []
  | x::[] -> []
  | x1::x2::t ->
     match x1 <= x2 with
     | true -> keep_increasing (x2::t)
     | false -> x2::t;;
keep_increasing [1;2;3;1];;
keep_increasing [1;1;1;1;];;

let rec keep_decreasing l =
  match l with
  | [] -> []
  | x::[] -> []
  | x1::x2::t ->
     match x1 >= x2 with
     | true -> keep_decreasing (x2::t)
     | false -> x2::t;;
keep_decreasing [4;3;1;2];;
keep_decreasing [1;1];;
keep_decreasing [];;

let unimodal l =
  match keep_increasing l with
  | [] -> true
  | rest -> (keep_decreasing rest) = [] ;;
unimodal [1;2;3;2;1];;
unimodal [1;1;2;];;
unimodal [2;1];;
unimodal [1;1;1];;
unimodal [1;2;4;5;2;3];;