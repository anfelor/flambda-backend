(* TEST
 flags += "-extension unique ";
 flags += "-extension borrowing ";
 flags += "-dsource ";
 expect;
*)

type record = { left : string; right : string }
[%%expect{|

type record = {
  left: string ;
  right: string };;
type record = { left : string; right : string; }
|}]

let cast_local : 'a @ local -> unit = fun x -> ()
[%%expect{|

let cast_local : local_ 'a -> unit = fun x -> ();;
val cast_local : local_ 'a -> unit = <fun>
|}]

let borrow_ident a = cast_local &a; ()
[%%expect{|

let borrow_ident a = cast_local (& a); ();;
val borrow_ident : 'a -> unit = <fun>
|}]

let borrow_mod_ident a = cast_local &List.map; ()
[%%expect{|

let borrow_mod_ident a = cast_local (& List.map); ();;
val borrow_mod_ident : 'a -> unit = <fun>
|}]

let borrow_constr a = cast_local &None; ()
[%%expect{|

let borrow_constr a = cast_local (& None); ();;
val borrow_constr : 'a -> unit = <fun>
|}]

let borrow_constr a = cast_local (&Some a); ()
[%%expect{|

let borrow_constr a = cast_local ((& Some) a); ();;
Line 1, characters 35-39:
1 | let borrow_constr a = cast_local (&Some a); ()
                                       ^^^^
Error: The constructor "Some" expects 1 argument(s),
       but is applied here to 0 argument(s)
|}]

(* This test is particularly interesting: The field projection is inside the
   borrow. That makes it possible to borrow fields of records where some parts
   (eg. 'right') have already been consumed. *)
let borrow_field a = cast_local &a.left; ()
[%%expect{|

let borrow_field a = cast_local (& a.left); ();;
val borrow_field : record -> unit = <fun>
|}]

let borrow_plus a b = cast_local (& a + b); ()
[%%expect{|

let borrow_plus a b = cast_local ((& a) + b); ();;
val borrow_plus : int -> int -> unit = <fun>
|}]

let borrow_plus a b = cast_local (a + & b); ()
[%%expect{|

let borrow_plus a b = cast_local (a + (& b)); ();;
val borrow_plus : int -> int -> unit = <fun>
|}]

let borrow_hash a b = cast_local (& a # b); ()
[%%expect{|

let borrow_hash a b = cast_local (& a#b); ();;
val borrow_hash : < b : 'a; .. > -> 'b -> unit = <fun>
|}]

(* Syntax error
let borrow_hash a b = cast_local (a # & b); ()
[%%expect{|
|}]
*)

let borrow_parens a = cast_local &(cast_local a); ()
[%%expect{|

let borrow_parens a = cast_local (& (cast_local a)); ();;
val borrow_parens : 'a -> unit = <fun>
|}]

let borrow_record_exp a = cast_local { &a with left = "" }; ()
[%%expect{|

let borrow_record_exp a = cast_local { (& a) with left = "" }; ();;
val borrow_record_exp : record -> unit = <fun>
|}]

(* Conflicts between borrowing and && *)

let borrow_vs_and a = cast_local ("" && a); ()
[%%expect{|

let borrow_vs_and a = cast_local ("" && a); ();;
Line 1, characters 34-36:
1 | let borrow_vs_and a = cast_local ("" && a); ()
                                      ^^
Error: This expression has type "string" but an expression was expected of type
         "bool"
|}]

let borrow_vs_and a = cast_local ("" &&& a); ()
[%%expect{|

let borrow_vs_and a = cast_local ("" &&& a); ();;
Line 1, characters 37-40:
1 | let borrow_vs_and a = cast_local ("" &&& a); ()
                                         ^^^
Error: Unbound value "(&&&)"
Hint: Did you mean "&&"?
|}]

let borrow_vs_and a = cast_local ("" &&a); ()
[%%expect{|

let borrow_vs_and a = cast_local ("" && a); ();;
Line 1, characters 34-36:
1 | let borrow_vs_and a = cast_local ("" &&a); ()
                                      ^^
Error: This expression has type "string" but an expression was expected of type
         "bool"
|}]

let borrow_vs_and a = cast_local ("" &&&a); ()
[%%expect{|

let borrow_vs_and a = cast_local ("" &&& a); ();;
Line 1, characters 37-40:
1 | let borrow_vs_and a = cast_local ("" &&&a); ()
                                         ^^^
Error: Unbound value "(&&&)"
Hint: Did you mean "&&"?
|}]

let borrow_vs_and a = cast_local ("" & &a); ()
[%%expect{|

let borrow_vs_and a = cast_local ("" (& (& a))); ();;
Line 1, characters 34-36:
1 | let borrow_vs_and a = cast_local ("" & &a); ()
                                      ^^
Error: This expression has type "string"
       This is not a function; it cannot be applied.
|}]

let borrow_vs_and a = cast_local ("" && &a); ()
[%%expect{|

let borrow_vs_and a = cast_local ("" && (& a)); ();;
Line 1, characters 34-36:
1 | let borrow_vs_and a = cast_local ("" && &a); ()
                                      ^^
Error: This expression has type "string" but an expression was expected of type
         "bool"
|}]

(* Syntax error:
let borrow_vs_and a = cast_local (true & &&a); ()
[%%expect{|
|}]
*)
