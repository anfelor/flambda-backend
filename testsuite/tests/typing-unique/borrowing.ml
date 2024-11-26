(* TEST
 flags += "-extension unique ";
 flags += "-extension borrowing ";
 expect;
*)

let unique_id : 'a @ unique -> 'a @ unique = fun x -> x
let global_id : 'a -> 'a = fun x -> x
let local_id : 'a @ local -> 'a @ local = fun x -> x
let cast_unique : 'a @ unique -> unit = fun x -> ()
let cast_global : 'a @ global -> unit = fun x -> ()
let cast_local : 'a @ local -> unit = fun x -> ()
type 'a pair = { left : 'a; right : 'a }
[%%expect{|
val unique_id : 'a @ unique -> 'a @ unique = <fun>
val global_id : 'a -> 'a = <fun>
val local_id : local_ 'a -> local_ 'a = <fun>
val cast_unique : 'a @ unique -> unit = <fun>
val cast_global : 'a -> unit = <fun>
val cast_local : local_ 'a -> unit = <fun>
type 'a pair = { left : 'a; right : 'a; }
|}]

(* Basic properties of borrowing *)

let borrow_is_local_okay a = cast_local &a
[%%expect{|
Line 1, characters 40-42:
1 | let borrow_is_local_okay a = cast_local &a
                                            ^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let borrow_is_local_bad a = cast_global &a
[%%expect{|
Line 1, characters 40-42:
1 | let borrow_is_local_bad a = cast_global &a
                                            ^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let borrow_requires_global_region_okay a = cast_local &a
[%%expect{|
Line 1, characters 54-56:
1 | let borrow_requires_global_region_okay a = cast_local &a
                                                          ^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let borrow_requires_global_region_bad a = stack_ (let x = &a in ())
[%%expect{|
Line 1, characters 49-67:
1 | let borrow_requires_global_region_bad a = stack_ (let x = &a in ())
                                                     ^^^^^^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

let borrow_may_be_unique_okay a = cast_unique &a
[%%expect{|
Line 1, characters 46-48:
1 | let borrow_may_be_unique_okay a = cast_unique &a
                                                  ^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let borrow_may_be_unique_bad a = cast_unique (&a, &a)
[%%expect{|
Line 1, characters 46-48:
1 | let borrow_may_be_unique_bad a = cast_unique (&a, &a)
                                                  ^^
Error: This value escapes its region.
|}]

let borrow_may_be_aliased_okay a = cast_local (&a, a)
[%%expect{|
Line 1, characters 47-49:
1 | let borrow_may_be_aliased_okay a = cast_local (&a, a)
                                                   ^^
Error: This value escapes its region.
|}]

let borrow_may_be_aliased_okay a = cast_local (a, &a)
[%%expect{|
Line 1, characters 50-52:
1 | let borrow_may_be_aliased_okay a = cast_local (a, &a)
                                                      ^^
Error: This value escapes its region.
|}]

let borrow_may_be_duplicated_okay a = cast_local (&a, &a)
[%%expect{|
Line 1, characters 50-52:
1 | let borrow_may_be_duplicated_okay a = cast_local (&a, &a)
                                                      ^^
Error: This value escapes its region.
|}]

let borrow_stays_once_bad (once_ a) = cast_local (&a, &a)
[%%expect{|
Line 1, characters 51-52:
1 | let borrow_stays_once_bad (once_ a) = cast_local (&a, &a)
                                                       ^
Error: This value is "once" but expected to be "many".
|}]

(* Projections *)

let borrowing_projection_okay a =
  let _l = &a.left in
  cast_unique &a.right
[%%expect{|
Line 3, characters 14-22:
3 |   cast_unique &a.right
                  ^^^^^^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let borrowing_projection_bad a =
  let _l = &a.left in
  cast_unique &a.left
[%%expect{|
Line 3, characters 14-21:
3 |   cast_unique &a.left
                  ^^^^^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let borrow_may_be_unique_okay a = cast_unique (&a.left, &a.right)
[%%expect{|
Line 1, characters 47-54:
1 | let borrow_may_be_unique_okay a = cast_unique (&a.left, &a.right)
                                                   ^^^^^^^
Error: This value escapes its region.
|}]

let borrow_may_be_unique_bad a = cast_unique (&a.left, &a.left)
[%%expect{|
Line 1, characters 46-53:
1 | let borrow_may_be_unique_bad a = cast_unique (&a.left, &a.left)
                                                  ^^^^^^^
Error: This value escapes its region.
|}]

let borrow_stays_once_okay (once_ a) = cast_local (&a.left, &a.right)
[%%expect{|
Line 1, characters 52-58:
1 | let borrow_stays_once_okay (once_ a) = cast_local (&a.left, &a.right)
                                                        ^^^^^^
Error: This value is "once" but expected to be "many".
|}]

let borrow_stays_once_bad (once_ a) = cast_local (&a.left, &a.left)
[%%expect{|
Line 1, characters 51-57:
1 | let borrow_stays_once_bad (once_ a) = cast_local (&a.left, &a.left)
                                                       ^^^^^^
Error: This value is "once" but expected to be "many".
|}]

(* Let-based regions *)

let borrow_could_escape a =
  let x = &a
  in x
[%%expect{|
Line 3, characters 5-6:
3 |   in x
         ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let borrow_does_not_escape a =
  let x = &a; ()
  in x
[%%expect{|
val borrow_does_not_escape : 'a -> unit = <fun>
|}]

let borrow_could_escape a =
  let x = global_id &a
  in x
[%%expect{|
Line 2, characters 20-22:
2 |   let x = global_id &a
                        ^^
Error: This value escapes its region.
|}]

let borrow_could_escape a =
  let x = global_id &a; ()
  in x
[%%expect{|
Line 2, characters 20-22:
2 |   let x = global_id &a; ()
                        ^^
Error: This value escapes its region.
|}]

let borrow_could_escape a =
  let x = local_id &a
  in x
[%%expect{|
Line 3, characters 5-6:
3 |   in x
         ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let borrow_does_not_escape a =
  let x = local_id &a; ()
  in x
[%%expect{|
val borrow_does_not_escape : 'a -> unit = <fun>
|}]

(* Match-based regions *)

let borrow_and_unique a =
  match &a with
   | { left; right } when right = "" -> cast_local left
   | { left; _ } -> cast_unique left
[%%expect{|
Line 3, characters 51-55:
3 |    | { left; right } when right = "" -> cast_local left
                                                       ^^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]
