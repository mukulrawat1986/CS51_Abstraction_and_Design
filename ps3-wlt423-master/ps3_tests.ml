
open Ps3 ;;
  
(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)
let _ = assert(negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []})
let _ = assert(negate {neg = true; coeffs = [1; 2]}
                    = {neg = false; coeffs = [1; 2]})


	        
(* Some advice on automated testing:

   Here is an automated testing function that checks every pair of
   integers between count and max to verify that the bignum
   representations of count and max are the same if and only if count
   and max are.

   Use this function to help you catch potential edge cases. While
   this kind of automated testing is helpful, it is still important
   for you to think about what cases may be difficult for your
   algorithm. Also, think about what inputs to run the testing
   function on. If you're having trouble isolating a bug, you can try
   printing out which values cause an assert failure.

   You may find that adding this sort of testing function for other
   functions is useful.  *)
	      
let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max ;;

(* Examples of using the automated testing function *)

let () = test_equal (-10000) 10000 ;;
let () = test_equal 10000 (-10000) ;;
let () = test_equal (-10000) 9999 ;;


assert(plus (fromInt ~-10) (fromInt 5)   = fromInt ~-5);;
assert(plus (fromInt 5) (fromInt ~-10)   = fromInt ~-5);;
assert(plus (fromInt ~-10) (fromInt ~-10)= fromInt ~-20);;
assert(plus (fromInt ~-25) (fromInt 15) = fromInt ~- 10);;
assert(plus (fromInt 1000) (fromInt 1000) = fromInt 200);;
assert(plus (fromInt 0) (fromInt 0) = fromInt 0);;

let a1 = fromInt 10000;;
let a2 = fromInt 20000;;
let b1 = fromInt ~-10000;;
let b2 = fromInt ~-20000;;
assert(less a1 a2 = greater a2 a1);;
assert(less a2 a1 = greater a1 a2);;
assert(less b1 a1 = greater a1 b1);;
assert(less b2 a1 = greater a1 b2);;
assert(less b1 b2 = greater b2 b1);;
assert(less b2 b1 = greater b1 b2);;
assert(less (fromInt 1000) (fromInt 2000) = true);;

assert(toInt (fromInt 123) = Some(123));;
assert(toInt (fromInt 123456) = Some(123456));;
assert(toInt (fromInt 123456789) = Some (123456789));;
assert(toInt (fromInt max_int) = Some (max_int));;
assert(toInt (fromInt min_int) = Some (min_int));;
assert(toInt {neg=false;coeffs=[123;456;789;123;456;789]} = None);;

assert(fromInt 123456 = {neg=false; coeffs=[123;456]});;
assert(fromInt 123456789 = {neg=false; coeffs=[123;456;789]});;
assert(fromInt (-123456) = {neg=true; coeffs=[123;456]});;
assert(fromInt (-123456789) = {neg=true;coeffs=[123;456;789]});;