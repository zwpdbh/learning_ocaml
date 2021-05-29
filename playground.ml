let _ = print_endline "Hello world!";;

4 + ( if 'a' = 'b' then 1 else 2);;

if 2 > 4 then 5 else 1;;

let x = 5 in 
   (let x = 6 in x) + x;;




(* mutually recursive function *)
let rec even n =
  n = 0 || odd(n-1)

and odd n =
  n != 0 && even(n-1);;
  

(*Annonymous function*)
let inc = fun x -> x + 1;;

(*pipeline*)
5 |> inc |> inc ;;

(*Labeld argument provide extra information for function type*)
let inc02  ~name1 ~name2 = name1 + name2;;

(*Optinal argument*)
let inc03 ?name:(arg1=9) arg2 = arg1 + arg2;;
inc03 ~name:49 100;;
inc03 1000;;

(*partial function*)
(*t1 -> t2 -> t3 -> t4 really means t1 -> (t2 -> (t3 -> t4)) *) 
let add x y = x + y;;
let add5 = add 5;;

(*operator as functions*)
( * ) 1 2;;
let ( ^^ ) x y = max x y;;
( ^^ ) 100 1000;;

(*debug using trace*)
let rec fib x = if x <= 1 then 1 else fib(x-1) + fib(x-2);;
#trace fib;;
(*debug using print*)
let testinc x y = 
  print_endline "Hello world!";
  x + y;;