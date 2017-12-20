(* 
			 CS 51 Problem Set 2
	    Higher Order Functional Programming -- Testing
			     Spring 2017
 *)

open Mapfold ;;
  
let test () =
  assert ((negate_all []) = []);;
  assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);;
  assert ((negate_all [-1; 2; -1]) = [1; -2; 1]);;
  assert ((sum [-1; 0; 1]) = 0);;
  assert ((sum []) = 0);;
  assert ((sum [-1; -1]) = -2);;
  assert ((sum_rows [[]]) = []);;
  assert ((sum_rows [[1; 2]; [3; 4]]) = [3; 7]);;
  assert ((sum_rows [[-1; 0;-1]; [0]]) = [0; 0]);;
  assert ((filter_odd [-1; 2; -1]) = [-1; 1]);;
  assert ((filter_odd []) = []);;
  assert ((filter_odd [2]) = []);;
  assert ((num_occurs (-1) [-1; 2; -1]) = 2);;
  assert ((num_occurs 0 [0; 0; 0]) = 3);;
  assert ((num_occurs  3 [-1; 2; -1]) = 0);;
  assert ((super_sum [[1; 2]; [3; 4]]) = 10);;
  assert ((super_sum [[]; []]) = 0);;
  assert ((super_sum [[0; 0]; [0; 0]]) = 0);;
  assert ((filter_range [-10; 0; 10; 100] (1, 11)) = [10]);;
  assert ((filter_range [-1; 0; 1] (-1, 1)) = [-1; 0; 1]);;
  assert ((filter_range [] (0, 0)) = []);;
  assert (floats_of_ints [0; 1; 2] = [0.; 1.; 2.] );;
  assert (floats_of_ints [-1] = [-1.] );;
  assert (floats_of_ints [] = [] );;
  assert (log10s [1.0; 0.0; -1.0] = [Some 0.; None; None]);;
  assert (log10s [-2.; -5.] = [None;None]);;
  assert (log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None]);;
  assert (deoptionalize [Some 3; None; Some 5; Some 10] = [3; 5; 10]);;
  assert (deoptionalize [None] = []);;
  assert (deoptionalize [Some 0] = [0]);;
  assert (some_sum [Some 0; Some 0; None; Some 0] = 0);;
  assert (some_sum [None; None; None] = 0);;
  assert (some_sum [Some 1; Some 2; None; Some 3] = 6);;
  assert (mult_odds [1; 3; 0; 2; -5] = -15);;
  assert (mult_odds [2; 4] = 0);;
  assert (mult_odds [1 ; 3; -1] = -3);;
  assert(concat [[1]; [3]; [5]] = [1; 3; 5]);;
  assert(concat [["CS"];["51"]] = ["CS"; "51"]);;
  assert(concat [["H"];["I"]] = ["H"; "I"]);;
  assert(filter_by_year [("Joe",2010);("Bob",2010);("Tom",2013)] 2010 =
	  ["Joe";"Bob"] );;
  assert(filter_by_year [("Joe",2010);("Bob",2010);("Tom",2013)] 2012 =
	  [] );;
  assert(filter_by_year [("Joe",2013)] 2013 =
	  ["Joe"]);;
  (*  Additional tests go here... *)


test();;
print_endline "All tests passed.";;
