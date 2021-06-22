(* Ref: https://ocaml.org/learn/tutorials/99problems.html *)


(*1. Write a function last : 'a list -> 'a option that returns the last element of a list. *)
let rec last_in_list lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _::t -> last_in_list t;;

(* About how to Option get, ref: https://stackoverflow.com/questions/12288628/ocaml-option-get*)
let get_last lst =
  match (last_in_list lst) with
  | Some x -> x
  | None -> raise (Invalid_argument "empty list");;
