(* TEST
 flags += "-extension unique ";
 flags += "-extension borrowing";
 expect;
*)

let cast_local : 'a @ local -> unit = fun x -> ()
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_ident a = cast_local &a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_mod_ident a = cast_local &List.map
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_constr a = cast_local &None
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_constr a = cast_local &Some a
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_field a = cast_local &a.left
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_parens a = cast_local &(global_id a)
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let borrow_record_exp a = cast_local { &a with left = "" }
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

(* Conflicts between borrowing and && *)

let borrow_vs_and a = cast_local (true && a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]

let borrow_vs_and a = cast_local (true &&& a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]

let borrow_vs_and a = cast_local (true &&a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]

let borrow_vs_and a = cast_local (true &&&a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]

let borrow_vs_and a = cast_local (true & &a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]

let borrow_vs_and a = cast_local (true && &a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]

let borrow_vs_and a = cast_local (true & &&a)
[%%expect{|
val aliased_id : 'a -> 'a = cast_local (<fun>)
|}]