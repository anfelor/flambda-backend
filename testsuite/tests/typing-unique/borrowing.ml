(* TEST
 flags += "-extension unique ";
 flags += "-extension borrowing";
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
val aliased_id : 'a -> 'a = <fun>
|}]

(* Basic properties of borrowing *)

let borrow_is_local_okay a = cast_local &a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_is_local_bad a = cast_global &a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_requires_global_region_okay a = cast_local &a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_requires_global_region_bad a = local_id &a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_unique_okay a = cast_unique &a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_unique_bad a = cast_unique (&a, &a)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_aliased_okay a = cast_local (&a, a)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_aliased_okay a = cast_local (a, &a)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_duplicated_okay a = cast_local (&a, &a)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_stays_once_bad (once_ a) = cast_local (&a, &a)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

(* Projections *)

let borrowing_projection_okay a =
  let l = &a.left in
  cast_unique &a.right
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrowing_projection_bad a =
  let l = &a.left in
  cast_unique &a.left
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_unique_okay a = cast_unique (&a.left, &a.right)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_may_be_unique_bad a = cast_unique (&a.left, &a.left)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_stays_once_okay (once_ a) = cast_local (&a.left, &a.right)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_stays_once_bad (once_ a) = cast_local (&a.left, &a.left)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

(* Let-based regions *)

let borrow_could_escape a =
  let x = &a
  in x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_does_not_escape a =
  let x = &a; ()
  in x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_could_escape a =
  let x = escaping_id &a
  in x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_could_escape a =
  let x = escaping_id &a; ()
  in x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_could_escape a =
  let x = local_id &a
  in x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_does_not_escape a =
  let x = local_id &a; ()
  in x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

(* Match-based regions *)

let borrow_and_unique a =
  match &a with
   | { l; r } when r = "" -> cast_local l
   | { l; _ } -> cast_unique l
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]