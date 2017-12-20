(*
			 CS 51 Problem Set 1
		Core Functional Programming -- Testing
			     Spring 2017
 *)			     

open Ps1 ;;

(* reversed tests *)
let () = assert ((reversed []) = true);;
let () = assert ((reversed [1]) = true);;
let () = assert ((reversed [0; 1]) = false);;
let () = assert ((reversed [1; 0]) = true);;
let () = assert ((reversed [1; 1]) = true);;
let () = assert ((reversed [1;0;-1]) = true);;

(* merge tests *)
let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;

(* unzip tests *)
let () = assert (unzip [(1,2);(2,1)] = ([1;2], [2;1]));;
let () = assert (unzip [] = ([],[]));;
let () = assert (unzip [(1,2)] = ([1], [2]));;	
let () = assert (unzip [(5,5);(-3,-3);(1,1)] = ([5;-3;1],[5;-3;1]));;

(* few_divisors tests *)
let () = assert (few_divisors 17 3 = true);;
let () = assert (few_divisors 4 3 = false);;
let () = assert (few_divisors 4 4 = true);;
let () = assert (few_divisors 0 0 = false);;
let () = assert (few_divisors 4 4 = true);;

(* concat_list tests *)
let () = assert (concat_list ", " ["Greg"; "Anna"; "David"] = "Greg, Anna, David");;
let () = assert (concat_list "..." ["Moo"; "Baaa"; "Quack"] = "Moo...Baaa...Quack");;
let () = assert (concat_list ", " [] = "") ;;
let () = assert (concat_list ", " ["Moo"] = "Moo");;
let () = assert (concat_list "" ["Greg"; "Greg"] ="GregGreg");;

(* to_run_length tests *)
let () = assert (to_run_length ['a';'b';'c';'c'] = [(1,'a');(1,'b');(2,'c')]);;
let () = assert (to_run_length [] = []);;
let () = assert (to_run_length ['a'] = [(1,'a')]);;

(* from_run_length tests *)
let () = assert (from_run_length (to_run_length ['a';'b';'c']) =  ['a';'b';'c']);;
let () = assert (from_run_length [(2,'a')] = ['a';'a']);;
let () = assert (from_run_length [(0,'a')] = []);;
let () = assert (from_run_length [] = []);;