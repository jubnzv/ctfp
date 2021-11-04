(* 1. Show that the terminal object is unique up to unique isomorphism. *)

(* Uniqueness up to isomorphism means that any two terminal objects are
   isomorphic. Consider two terminal objects `t1` and `t2`. Since `t1` is
   terminal, there is a unique morphism `f` from `t2` to `t1`. In the same way
   we have a unique morphism `g` from `t1` to `t2`. The composition `f . g`
   must be a morhphism from `t1` to `t1`. But `t1` is terminal, so there can
   only be one morphism going from `t1` to `t1`, therefore `f . g` is equal to
   identity. Similary, `g . f` is an identity for `t2`. This proves that `f`
   and `g` must be the inverse of each other.  *)

(* 2. What is a product of two objects in a poset? Hint: Use the universal
   construction. *)

(* 3. What is a coproduct of two objects in a poset? *)

(* 4. Implement the equivalent of Haskell Either as a generic type in
   your favorite language (other than Haskell). *)
type ('a, 'b) either = Left of 'a | Right of 'b

(* 5. Show that Either is a “better” coproduct than int equipped with
   two injections:
     int i(int n) { return n; }
     int j(bool b) { return b ? 0: 1; }
   Hint: Define a function
     int m(Either const & e);
   that factorizes i and j. *)
let i : int -> int =
  fun n -> n
let j : bool -> int =
  fun b -> if b then 0 else 1
let m : ((int, bool) either) -> int =
  fun e -> match e with
    | Left n -> n
    | Right b -> if b then 0 else 1

(* 6. Continuing the previous problem: How would you argue that int with the
   two injections i and j cannot be “better” than Either? *)

(* 7. Still continuing: What about these injections? *)

(* 8. Come up with an inferior candidate for a coproduct of int and bool that
   cannot be better than Either because it allows multiple *)
