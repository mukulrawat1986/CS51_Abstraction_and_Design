open Expression ;;
open ExpressionLibrary ;;

let test () =
  assert (contains_var (parse "x+3"));;
  assert (not (contains_var (parse "2")));;
  assert ( contains_var (parse "5-1") = false);;
  assert(evaluate (parse "x^4 + 3") 2.0 = 19.0 );;
  assert(evaluate (parse "(2^x)+2") 2.0 = 6.);;
  assert(evaluate (parse "(cos x)/(cos x)") 1. = 1.);;
  assert(to_string (derivative (parse "10")) = "0.");;
  assert(to_string (derivative (parse "x")) = "1.");;
  assert(to_string (derivative (parse "x^2")) = "((2.*1.)*(x^(2.-1.)))" );;
  assert(to_string (derivative (parse "x*x")) = "((x*1.)+(1.*x))" );;
  assert(to_string (derivative (parse "x/x")) = "(((1.*x)-(x*1.))/(x^2.))");;

  (*  Additional tests go here... *)

test();;
print_endline "All tests passed.";;
