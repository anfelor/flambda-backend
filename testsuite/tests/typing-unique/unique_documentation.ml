(* TEST
   flags += "-extension unique ";
   flags += "-extension-universe alpha";
   expect;
*)

(*******************************)
(* Examples from documentation *)

type t = Con of { field : int }

let free : t @ unique -> unit = fun t -> ()
let free_field (unique_ i) = ()
let store : t @ aliased -> unit = fun t -> ()
let store_field i = ()
let flip_coin () = true
[%%expect{|
type t = Con of { field : int; }
val free : unique_ t -> unit @@ global many = <fun>
val free_field : ('a : value_or_null). unique_ 'a -> unit @@ global many =
  <fun>
val store : t -> unit @@ global many = <fun>
val store_field : ('a : value_or_null). 'a -> unit @@ global many = <fun>
val flip_coin : unit -> bool @@ global many = <fun>
|}]

let test () =
  let dup : t -> t * t @ aliased = function t -> t, t in
  let delay_free : t @ unique -> (unit -> unit) @ once = function t -> fun () -> free t in
  let alias : 'a @ unique -> 'a @ aliased = fun x -> x in
  let linearize : 'a @ many -> 'a @ once = fun x -> x in
  ()
[%%expect{|
Line 2, characters 6-9:
2 |   let dup : t -> t * t @ aliased = function t -> t, t in
          ^^^
Warning 26 [unused-var]: unused variable dup.

Line 3, characters 6-16:
3 |   let delay_free : t @ unique -> (unit -> unit) @ once = function t -> fun () -> free t in
          ^^^^^^^^^^
Warning 26 [unused-var]: unused variable delay_free.

Line 4, characters 6-11:
4 |   let alias : 'a @ unique -> 'a @ aliased = fun x -> x in
          ^^^^^
Warning 26 [unused-var]: unused variable alias.

Line 5, characters 6-15:
5 |   let linearize : 'a @ many -> 'a @ once = fun x -> x in
          ^^^^^^^^^
Warning 26 [unused-var]: unused variable linearize.

val test : unit -> unit @@ global many = <fun>
|}]

let okay t =
  match t with
  | Con { field } -> free t
[%%expect{|
val okay : unique_ t -> unit @@ global many = <fun>
|}]

let bad t =
  match t with
  | Con { field } ->
    free_field field;
    free t
[%%expect{|
Line 5, characters 9-10:
5 |     free t
             ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 4, characters 15-20:
4 |     free_field field;
                   ^^^^^

|}]

let okay t =
  match t with
  | Con { field } ->
    if flip_coin ()
    then free_field field
    else free t
[%%expect{|
val okay : unique_ t -> unit @@ global many = <fun>
|}]

let okay t =
  match t with
  | Con { field } ->
    store_field field;
    store t
[%%expect{|
val okay : t -> unit @@ global many = <fun>
|}]

let module_ret_unique =
  let mk () = Con { field = 1 } in
  let use () = free (mk ()) in
  ()
[%%expect{|
Line 3, characters 6-9:
3 |   let use () = free (mk ()) in
          ^^^
Warning 26 [unused-var]: unused variable use.

val module_ret_unique : unit @@ global many = ()
|}]

module Mk = struct
  let mk () = Con { field = 1 }
end

let module_ret_unique =
  let use () = free (Mk.mk ()) in
  ()
[%%expect{|
module Mk : sig val mk : unit -> t @@ global many portable end
Line 6, characters 20-30:
6 |   let use () = free (Mk.mk ()) in
                        ^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
|}]

module Unique_array = struct
  let set : 'a @ unique -> int -> 'b -> 'a @ unique = fun arr -> fun i -> fun x -> arr
  let size arr = 10
end
[%%expect{|
module Unique_array :
  sig
    val set :
      ('a : value_or_null) ('b : value_or_null).
        unique_ 'a -> int -> 'b -> unique_ 'a
      @@ global many portable
    val size : ('a : value_or_null). 'a -> int @@ global many portable
  end
|}]

let set_all_zero arr =
  for i = 0 to Unique_array.size arr do
    Unique_array.set arr i 0
  done
[%%expect{|
Line 3, characters 21-24:
3 |     Unique_array.set arr i 0
                         ^^^
Error: This value is "aliased" but expected to be "unique".
  Hint: This identifier cannot be used uniquely,
  because it was defined outside of the for-loop.
|}]

let set_all_zero arr =
  let set = Unique_array.set arr in
  for i = 0 to Unique_array.size arr do
    set i 0
  done
[%%expect{|
Line 4, characters 4-7:
4 |     set i 0
        ^^^
Error: The value "set" is once, so cannot be used inside a for loop
|}]

let set_all_zero arr =
  let size (unique_ arr) = 10, arr in
  let rec loop idx arr =
    if idx == 0 then arr
    else loop (idx - 1) (Unique_array.set arr idx 0)
  in
  let size, arr = size arr in
  loop size arr
[%%expect{|
val set_all_zero : ('a : value_or_null). unique_ 'a -> 'a @@ global many =
  <fun>
|}]

let check_tuple x y z =
  let m =
    match x, y, z with
    | p, q, r -> free x
  in m, y
[%%expect{|
val check_tuple :
  ('a : value_or_null) ('b : value_or_null).
    unique_ t -> 'a -> 'b -> unit * 'a
  @@ global many = <fun>
|}]

let check_tuple x y z =
  let m =
    match x, y, z with
    | p, q, r as t -> free x
  in m, y
[%%expect{|
Line 4, characters 27-28:
4 |     | p, q, r as t -> free x
                               ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 10-11:
3 |     match x, y, z with
              ^

|}]
