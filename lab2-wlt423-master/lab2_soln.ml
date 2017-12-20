# 1 "lab2.mlpp"
(* 
                              CS51 Lab 2
                    Polymorphism and record types
                             Spring 2017
 *)

# 8
(*
                               SOLUTION
 *)

# 13
(*
Objective:

In this lab, you'll exercise your understanding of polymorphism and
record types. Some of the problems extend those from Lab 1, but we'll
provide the necessary background code from that lab.

During the lab session, we recommend that you work on the following
exercises in this order:

1-5; 7; 9-11; 16-17

and complete the remaining ones at your leisure.
 *)

(*======================================================================
Part 1: Currying and uncurrying

........................................................................
Exercise 1: In this exercise, you'll define higher-order functions
curry and uncurry for currying and uncurrying binary functions
(functions of two arguments). The functions are named after
mathematician Haskell Curry '1920. (By way of reminder, a curried
function takes its arguments one at a time. An uncurried function
takes them all at once in a tuple.)

To think about before you start coding:

  * What should the types of curry and uncurry be?

  * What is an example of a function that curry could apply to?
    Uncurry?

  * What are some tests that you could try to verify that your
    implementations of curry and uncurry work properly?

Now implement the two functions curry and uncurry.
......................................................................*)

# 55
let curry f x y = f (x, y) ;;
     
# 61
let uncurry f (x, y) = f x y ;;

# 64
(*......................................................................
Exercise 2: OCaml's built in binary operators, like (+) and ( * ) are
curried:

# ( + ) ;;
- : int -> int -> int = <fun>
# ( * ) ;;
- : int -> int -> int = <fun>

Using your uncurry function, define uncurried plus and times
functions.
......................................................................*)

# 81
let plus = uncurry ( + ) ;;
     
  
# 88
  let times = uncurry ( * ) ;;
  
# 91
(*......................................................................
Exercise 3: Recall the prods function from Lab 1:
#endif
let rec prods (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x, y) :: tail -> (x * y) :: (prods tail) ;;

Now reimplement prods using map and your uncurried times function. Why
do you need the uncurried times function?
......................................................................*)

# 107
let prods =
  List.map times ;;

# 111
(*======================================================================
Part 2: Option types

In Lab 1, you implemented a function max_list that returns the maximum
element in a non-empty integer list. Here's a possible implementation
for max_list:

let rec max_list (lst : int list) : int =
  match lst with
  | [elt] -> elt
  | head :: tail -> let max_tail = max_list tail in
      if head > max_tail then head else max_tail ;;

As written, this function generates a warning that the match is not
exhaustive. Why? What's an example of the missing case? Try entering
the function in ocaml and see what information you can glean from the
warning.

The problem is that there is no reasonable value for the maximum
element in an empty list. This is an ideal application for option
types.

........................................................................
Exercise 4: 

Reimplement max_list, but this time, it should return an int option
instead of an int.
......................................................................*)

# 144
let rec max_list (lst: int list) : int option =
  match lst with
  | [] -> None
  | head :: tail ->
     match (max_list tail) with
     | None -> Some head
     | Some max_tail -> Some (if head > max_tail then head else max_tail);;
  
# 153
(*......................................................................
Exercise 5: Write a function to return the smaller of two int options,
or None if both are None. If exactly one argument is None, return the
other.  The built-in function min from the Pervasives module may be
useful.
......................................................................*)

# 164
let min_option (x: int option) (y: int option) : int option =
  match x, y with
  | None, None -> None
  | None, Some right -> Some right
  | Some left, None -> Some left
  | Some left, Some right -> Some (min left right) ;;
     
# 172
(*......................................................................
Exercise 6: Write a function to return the larger of two int options, or
None if both are None. If exactly one argument is None, return the
other.
......................................................................*)

# 182
let max_option (x: int option) (y: int option) : int option =
  match x, y with
  | None, None -> None
  | None, Some right -> Some right
  | Some left, None -> Some left
  | Some left, Some right -> Some (max left right) ;;

# 190
(*======================================================================
Part 3: Polymorphism practice

........................................................................
Exercise 7: Do you see a pattern in your implementations of min_option
and max_option? How can we factor out similar code?  

Write a higher-order function for binary operations on options taking
three arguments in order: the binary operation (a curried function)
and its first and second argument. If both arguments are None, return
None.  If one argument is (Some x) and the other argument is None,
function should return (Some x). If neither argument is none, the
binary operation should be applied to the argument values and the
result appropriately returned. 

What is calc_option's function signature? Implement calc_option.
......................................................................*)

# 212
let calc_option (f: 'a -> 'a -> 'a) (x: 'a option) (y: 'a option)
              : 'a option =
  match x, y with
  | None, None -> None
  | None, Some right -> Some right
  | Some left, None -> Some left
  | Some left, Some right -> Some (f left right) ;;
     
# 221
(*......................................................................
Exercise 8: Now rewrite min_option and max_option using the higher-order
function calc_option. Call them min_option_2 and max_option_2.
......................................................................*)
  
# 230
let min_option_2 : int option -> int option -> int option =
  calc_option min ;;
     
# 238
let max_option_2 : int option -> int option -> int option =
  calc_option max ;;

# 242
(*......................................................................
Exercise 9: Now that we have calc_option, we can use it in other
ways. Because calc_option is polymorphic, it can work on things other
than int options. Define a function and_option to return the boolean
AND of two bool options, or None if both are None. If exactly one is
None, return the other.
......................................................................*)
  
# 254
let and_option : bool option -> bool option -> bool option =
  calc_option (&&) ;;
  
# 258
(*......................................................................
Exercise 10: In Lab 1, you implemented a function zip that takes two
lists and "zips" them together into a list of pairs. Here's a possible
implementation of zip (here renamed zip_exn to distinguish it
from the zip you'll implement below, which has a different signature):

let rec zip_exn (x : int list) (y : int list) : (int * int) list =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip_exn xtl ytl) ;;

As implemented, this function works only on integer lists. Revise your
solution to operate polymorphically on lists of any type. What is the
type of the result?
......................................................................*)

# 278
let rec zip_exn (x: 'a list) (y: 'b list) : ('a * 'b) list =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip_exn xtl ytl) ;;

# 284
(*......................................................................
Exercise 11: Another problem with the implementation of zip_exn is that,
once again, its match is not exhaustive and it raises an exception
when given lists of unequal length. How can you use option types to
generate an alternate solution without this property? 

Do so below in a new definition of zip.
......................................................................*)

# 297
let rec zip (x : 'a list) (y : 'b list) : (('a * 'b) list) option =
  match (x, y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl ->
     (match zip xtl ytl with
      | None -> None
      | Some ztl -> Some ((xhd, yhd) :: ztl))
  | _, _ -> None ;;

# 307
(*====================================================================
Part 4: Factoring out None-handling

Recall the definition of dot_prod from Lab 1. Here it is adjusted to
an option type:

let dotprod (a : int list) (b : int list) : int option =
  let pairsopt = zip a b in
  match pairsopt with
  | None -> None
  | Some pairs -> Some (sum (prods pairs)) ;;

Also recall zip from Exercise 8 above.

Notice how in these functions we annoyingly have to test if a value of
option type is None, requiring a separate match, and passing on the
None value in the "bad" branch or introducing the Some in the "good"
branch. This is something we're likely to be doing a lot of. Let's
factor that out to simplify the implementation.

........................................................................
Exercise 12: Define a function called maybe that takes an argument of
type 'a option and a function of type 'a -> 'b, and "maybe" (depending
on whether its argument is a None or a Some) applies the function to
the argument. The maybe function either passes on the None if its
first argument is None, or if its first argument is Some v, it applies
its second argument to that v and returns the result, appropriately
adjusted for the result type. Implement the maybe function.
......................................................................*)
  
# 341
let maybe (x: 'a option) (f: 'a -> 'b) : 'b option =
  match x with 
  | None -> None
  | Some v -> Some (f v) ;;

# 347
(*......................................................................
Exercise 13: Now reimplement dotprod to use the maybe function. (The
previous implementation makes use of functions sum and prods. You've
already (re)implemented prods above. We've provided sum for you
below.)  Your new solution for dotprod should be much simpler than in
Lab 1.
......................................................................*)

let sum : int list -> int =
  List.fold_left (+) 0 ;;

# 362
let dotprod (a : int list) (b : int list) : int option =
  maybe (zip a b) (fun pairs -> sum (prods pairs)) ;;

# 366
(*......................................................................
Exercise 14: Reimplement zip along the same lines, in zip_2 below. 
......................................................................*)

# 375
let rec zip_2 (x : int list) (y : int list) : ((int * int) list) option =
  match (x, y) with
  | ([], []) -> Some []
  | (xhd :: xtl, yhd :: ytl) ->
     maybe (zip_2 xtl ytl)
           (fun ztl -> ((xhd, yhd) :: ztl))
  | (_, _) -> None ;;

# 384
(*......................................................................
Exercise 15: For the energetic, reimplement max_list along the same
lines. There's likely to be a subtle issue here, since the maybe
function always passes along the None.
......................................................................*)

# 394
let rec max_list (lst : int list) : int option =
  match lst with
  | [] -> None
  | [single] -> Some single
  | head :: tail ->
     maybe (max_list tail)
           (fun max_tail -> if head > max_tail then head else max_tail) ;;

# 403
(*======================================================================
Part 5: Record types

A college wants to store student records in a simple database,
implemented as a list of individual course enrollments. The
enrollments themselves are implemented as a record type, called
"enrollment", with string fields labeled "name" and "course" and an
integer student id number labeled "id". An appropriate type might be:
*)

type enrollment = { name : string;
                    id : int;
                    course : string } ;;

(* Here's an example of a list of enrollments. *)

let college = 
  [ { name = "Pat";   id = 1; course = "cs51" };
    { name = "Pat";   id = 1; course = "emr11" };
    { name = "Kim";   id = 2; course = "emr11" };
    { name = "Kim";   id = 2; course = "cs20" };
    { name = "Sandy"; id = 5; course = "ls1b" };
    { name = "Pat";   id = 1; course = "ec10b" };
    { name = "Sandy"; id = 5; course = "cs51" };
    { name = "Sandy"; id = 2; course = "ec10b" }
  ] ;;

(* In the following exercises, you'll want to avail yourself of the
List module functions, writing the requested functions in higher-order
style rather than handling the recursion yourself.

........................................................................
Exercise 16: Define a function called transcript that takes an
enrollment list and returns a list of all the enrollments for a given
student as specified with his or her id.

For example: 
# transcript college 5 ;;
- : enrollment list =
[{name = "Sandy"; id = 5; course = "ls1b"};
 {name = "Sandy"; id = 5; course = "cs51"}]
......................................................................*)

# 452
let transcript (enrollments: enrollment list)
               (student: int)
             : enrollment list =
  List.filter (fun { id; _ } -> id = student) enrollments ;;

(* Note the use of field punning, using the id variable to refer to
the valure of the id field.

An alternative approach is to use the dot notation to pick out the
record field. 

    let transcript (enrollments: enrollment list)
                   (student: int)
                 : enrollment list =
      List.filter (fun studentrec -> studentrec.id = student)
                  enrollments ;;
 *) 
  
# 471
(*......................................................................
Exercise 17: Define a function called ids that takes an enrollment list
and returns a list of all the id numbers in that enrollment list,
eliminating any duplicates. The function List.sort_uniq may be useful
here.

For example:
# ids college ;;
- : int list = [1; 2; 5]
......................................................................*)

# 486
let ids (enrollments: enrollment list) : int list =
  List.sort_uniq (compare)
                 (List.map (fun student -> student.id) enrollments) ;;

(* Note that here we use the alternative strategy of picking out the
id using dot notation. *)
  
# 494
(*......................................................................
Exercise 18: Define a function called verify that determines whether all
the entries in an enrollment list for each of the ids appearing in the
list have the same name associated.

For example: 
# verify college ;;
- : bool = false
......................................................................*)

# 508
let names (enrollments: enrollment list) : string list =
  List.sort_uniq (compare)
                 (List.map (fun { name; _ } -> name) enrollments) ;;
  
let verify (enrollments: enrollment list) : bool =
  List.for_all (fun l -> (List.length l) = 1)
               (List.map
                  (fun student -> names (transcript enrollments student))
                  (ids enrollments)) ;;
