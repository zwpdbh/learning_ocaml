type nat = Zero | Succ of nat

let zero = Zero ;;
let one = Succ zero;;
let two   = Succ one;;
let three = Succ two;;
let four  = Succ three;;