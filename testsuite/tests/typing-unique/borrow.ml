(* TEST
   flags += "-extension unique ";
   flags += "-extension borrowing ";
   expect;
*)

let unique_use (local_ unique_ _x) = ()
[%%expect{|
val unique_use : local_ 'a @ unique -> unit = <fun>
|}]

let global_shared_use : 'a -> unit = fun _ -> ()
[%%expect{|
val global_shared_use : 'a -> unit = <fun>
|}]

let local_shared_use (local_ a) = ()
[%%expect{|
val local_shared_use : local_ 'a -> unit = <fun>
|}]

let unique_shared_use (local_ unique_ x) (local_ y) = ()
[%%expect{|
val unique_shared_use : local_ 'a @ unique -> local_ 'b -> unit = <fun>
|}]

let shared_unique_use (local_ x) (local_ unique_ y) = ()
[%%expect{|
val shared_unique_use : local_ 'a -> local_ 'b @ unique -> unit = <fun>
|}]

let shared_shared_use (local_ x) (local_ y) = ()
[%%expect{|
val shared_shared_use : local_ 'a -> local_ 'b -> unit = <fun>
|}]

let local_returning (local_ x) = [%exclave] x
[%%expect{|
val local_returning : local_ 'a -> local_ 'a = <fun>
|}]


(* Cannot borrow at top level let *)
let x = & "hello"
[%%expect{|
val x : string = "hello"
|}]

(* borrowed values are shared and cannot be used as unique *)
let foo () =
  let unique_ y = "hello" in
  unique_use &y;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let x = "hello" in
  global_shared_use &x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* In the borrow region, you are not allowed to use the original value uniquely
   , whether that unique usage is before or after borrowing *)
let foo () =
  let x = "hello" in
  let y = &x in
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 11-12:
3 |   let y = &x in
               ^

|}]

let foo () =
  let x = "hello" in
  let y = (unique_use x; &x) in
  ()
[%%expect{|
Line 3, characters 26-27:
3 |   let y = (unique_use x; &x) in
                              ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 22-23:
3 |   let y = (unique_use x; &x) in
                          ^

|}]

(* Due to our implementation, borrowing is still counted even if you don't bind
   the borrowed value *)
let foo () =
  let x = "hello" in
  let _ = &x in
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 11-12:
3 |   let _ = &x in
               ^

|}]

(* In the borrow region, you can use the original value as shared, whether the usage
is before or after the borrowing *)
let foo () =
  let x = "hello" in
  let _y = &x in
  global_shared_use x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let x = "hello" in
  let _y = (global_shared_use x; &x) in
  ();
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* But that shared usage ruins the borrowing, so you can't uniquely use the
   value even after the borrow region. *)
let foo () =
  let x = "hello" in
  (let y = &x in
  global_shared_use x);
  unique_use x;
  ()
[%%expect{|
Line 5, characters 13-14:
5 |   unique_use x;
                 ^
Error: This value is used here as unique,
       but it has already been used more than once:
Line 3, characters 12-13:
3 |   (let y = &x in
                ^

|}]

(* Typical usage - borrowed followed by unique is ok. *)
let foo () =
  let x = "hello" in
  (let y = &x in
  local_shared_use y);
  unique_use x
[%%expect{|
Line 5, characters 13-14:
5 |   unique_use x
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 12-13:
3 |   (let y = &x in
                ^

|}]


(* nested borrowing - two regions below,
z cannot escape its region
*)
let foo () =
  let x = "hello" in
  let y = & x in
  ignore (let z = & y in z)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* y can escape *)
let foo () =
  let x = "hello" in
  let y = & x in
  ignore (let z = &y in y)
[%%expect{|
Line 4, characters 14-15:
4 |   ignore (let z = &y in y)
                  ^
Warning 26 [unused-var]: unused variable z.

val foo : unit -> unit = <fun>
|}]


(* Borrowing in Texp_match/Texp_apply/Texp_let are very similar,
   there is no need to duplicate every test for each of them.
   Below we only test the common cases. *)

(* borrowed values are shared and cannot be used as unique *)
let foo () =
  let y = "hello" in
  match & y with
  | x -> ignore (unique_use x)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let unique_ y = "hello" in
  match & y with
  | x -> ignore (global_shared_use x)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* During borrowing, you are not allowed to use the original value uniquely *)
let foo () =
  let x = "hello" in
  match & x with
  | _y -> ignore (unique_use x)
[%%expect{|
Line 4, characters 29-30:
4 |   | _y -> ignore (unique_use x)
                                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 10-11:
3 |   match & x with
              ^

|}]


(* Due to our implementation, borrowing is still counted even if you don't
really bind the borrowed value *)
let foo () =
  let x = "hello" in
  match & x with
  | _ -> ignore (unique_use x)

[%%expect{|
Line 4, characters 28-29:
4 |   | _ -> ignore (unique_use x)
                                ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 10-11:
3 |   match & x with
              ^

|}]

(* but shared use is fine *)
let foo () =
  let x = "hello" in
  match & x with
  | _ -> ignore (global_shared_use x)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Moreover, that shared use will clash with later unique use *)
let foo () =
  let x = "hello" in
  (match & x with
  | _ -> global_shared_use x
  );
  ignore (unique_use x)
[%%expect{|
Line 6, characters 21-22:
6 |   ignore (unique_use x)
                         ^
Error: This value is used here as unique,
       but it has already been used more than once:
Line 3, characters 11-12:
3 |   (match & x with
               ^

|}]

(* function application borrowing *)

(* You can't use x uniquely after shared usage *)
let foo () =
  let unique_ x = "hello" in
  global_shared_use x;
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 20-21:
3 |   global_shared_use x;
                        ^

|}]

(* unless if you borrow it *)
let foo () =
  let unique_ x = "hello" in
  local_shared_use &x;
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 20-21:
3 |   local_shared_use &x;
                        ^

|}]

(* borrow after unique usage is bad *)
let foo () =
  let unique_ x = "hello" in
  unique_use x;
  local_shared_use &x;
  ()
[%%expect{|
Line 4, characters 20-21:
4 |   local_shared_use &x;
                        ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-14:
3 |   unique_use x;
                 ^

|}]

(* multiple borrowing is fine *)
let foo () =
  let unique_ x = "hello" in
  shared_shared_use &x &x;
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique,
       but it has already been used more than once:
Line 3, characters 21-22:
3 |   shared_shared_use &x &x;
                         ^

|}]

(* but you need to borrow both of course *)
let foo () =
  let unique_ x = "hello" in
  shared_shared_use &x x;
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique,
       but it has already been used more than once:
Line 3, characters 21-22:
3 |   shared_shared_use &x x;
                         ^

|}]

(* a tricky case *)
let foo () =
  let unique_ x = "hello" in
  local_shared_use (& (global_shared_use x));
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 41-42:
3 |   local_shared_use (& (global_shared_use x));
                                             ^

|}]

(* borrowed values are shared *)
let foo () =
  let unique_ x = "hello" in
  unique_use &x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let unique_ x = "hello" in
  global_shared_use &x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Borrowing doesn't work well with local-returning functions *)
let foo () =
  let unique_ x = "hello" in
  local_returning &x;
  ()
[%%expect{|
Line 3, characters 2-20:
3 |   local_returning &x;
      ^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

val foo : unit -> unit = <fun>
|}]

let foo () =
  let unique_ x = "hello" in
  shared_unique_use &x x;
  ()
[%%expect{|
Line 3, characters 23-24:
3 |   shared_unique_use &x x;
                           ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 21-22:
3 |   shared_unique_use &x x;
                         ^

|}]


(* Closing over borrowing *)

(* In the following, [bar] is never called, but it closes over borrowing,
   and thus is also borrowing. *)
let foo () =
  let x = "hello" in
  let bar () =
    let bar' () =
      local_shared_use &x;
      ()
    in
    ()
  in
  unique_use x
[%%expect{|
Line 10, characters 13-14:
10 |   unique_use x
                  ^
Error: This value is used here as unique, but it has already been used:
Line 5, characters 24-25:
5 |       local_shared_use &x;
                            ^

|}]

(* Unique use is allowed after leaving the borrow region *)
let foo () =
  let x = "hello" in
  let _z =
    (let _bar () =
      local_shared_use &x;
      ()
     in
    42)
  in
  unique_use x
[%%expect{|
Line 10, characters 13-14:
10 |   unique_use x
                  ^
Error: This value is used here as unique, but it has already been used:
Line 5, characters 24-25:
5 |       local_shared_use &x;
                            ^

|}]

(* The function is local  *)
let foo () =
  let x = "hello" in
  let bar () =
    local_shared_use &x;
    ()
  in
  ref bar
[%%expect{|
val foo : unit -> (unit -> unit) ref = <fun>
|}]

(* The function is shared *)
let foo () =
  let x = "hello" in
  let bar () =
    local_shared_use &x;
    ()
  in
  unique_use bar;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]
