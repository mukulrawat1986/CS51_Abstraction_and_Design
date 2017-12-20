(* CS51 Spring 2017
 * PS 0 *)

(*
 * This problem set is a short script that prints out a 
 * survey and, if you wish, posts a message to a web server
 * so that you can see OCaml in action. 
 * Posting a message to the server is optional, and  
 * is only availabe as an exercise to those students
 * who have an @college.harvard.edu email address 
 * (required for authentication). 
 * All other students leave the post flag set to false. 
 * 
 * To receive a key, visit http://pset0.ml/ and log in.
 *)



(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by typing "ocamlbuild ps0.byte" in the terminal. *)

(* 1.a. Replace FIRST and LAST with your first and last name *)
let name : (string * string) = ("Walter", "Thornton");;

(* 1.b. Replace "Other ..." in class_year with your current year that is of
 * type 'year' *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Senior;;

(* 1.c. Replace "Other ..." in took_cs_50 with whether or not you took CS50,
 * with a response of *)
type cs50 = Took | DidNotTake | Other of string;;

let took_cs_50 : cs50 = Took;;

(* 1.d. Replace "Other ..." in my_system the operating system
 * of your computer. *)
type system = Mac | Windows10 | Windows7 | Linux | Other of string;;

let my_system : system = Windows10;;

(* 1.e. Replace the string below with a message for the problem set website. *)
let exciting : string = "I'm excited about ???";;


(* 1.f. Set this variable to true if you want
 * your information to appear on http://pset0.ml.
 * Reminder: only available for students with 
 * @college.harvard.edu email addresses. *)
let post : bool = false;;

(* 1.g. Set this variable to the key you got from pset0.ml 
 * if you want your information to appear there. *)
let key : string = "";;

(*======================================================================
 * 1.h. Time estimate
 * Please give us an honest (if approximate) estimate of how long (in
 * minutes) this problem set took you to complete including both following the
 * setup instructions and filling out this file. We care about your responses 
 * and will use them to help guide us in creating future assignments.
 *....................................................................*)

let minutes_spent () : int = 180 ;;

(* ***
   2. You shouldn't change anything below this line, but you should
   read to the bottom of the file and try to figure out what is going on.
  **** *)

let print = Printf.printf;;

let convert_survey () =
  let (first, last) = name in
  let string_year =
    match class_year with
       | Freshman -> "2020"
       | Sophomore -> "2019"
       | Junior -> "2018"
       | Senior -> "2017"
       | Other s -> "Other: " ^ s
    in
  let string_cs50 =
    match took_cs_50 with
      | Took -> "I took CS50"
      | DidNotTake -> "I did not take CS50"
      | Other s -> "Other: " ^ s in
    (first, last, string_year, string_cs50, exciting, key)

let print_survey (first, last, year, cs50, message, _) =
    (print "----------------------------------------\n";
     print "Name: %s %s\n\n" first last;
     print "Year: %s\n\n" year;
     print "50?: %s \n\n" cs50;
     print "%s\n\n" message;
     print "----------------------------------------\n\n";);;

let post_survey s =
  let (first, last, year, cs50, message, key) = s in
  let open Nethttp_client.Convenience in
  let mbody = [("first", first); ("last", last); ("year", year); ("cs50", cs50);
               ("message", message); "key", key] in
  let url = "http://pset0.ml" in
  let open Yojson.Basic in
  http_post url mbody 
    |> fun x -> from_string x
    |> Util.member "status" 
    |> fun x -> to_string x |> print "%s\n" ;
  print_survey s;;

let s = convert_survey () in 
(if post then post_survey else print_survey) s;;

(* type "make all" to compile the file.
 * (This command invokes "ocamlbuild",
 * which is the smart OCaml compiler we use 
 * in this course -- more on that later!)
 * Then type ./ps0.byte to run the program and print the output.
 * Make sure all the values look right, and visit http://pset0.ml/responses/ to see the messages.
 * If everything looks correct, submit and
 * you're done! *)
