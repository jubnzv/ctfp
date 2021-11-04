(* Signatures from the CTFP OCaml edition. *)
module type Functor = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
module type T = sig
  type t
end

(* 1. Can we turn the Maybe type constructor into a functor by defining:
     fmap _ _ = Nothing
   which ignores both of its arguments? (Hint: Check the functor laws.) *)

(* No, because identity law won't work in that case:
     fmap id (Just a) == Nothing *)

(* 2. Prove functor laws for the reader functor. Hint: it’s really simple. *)

(* 3. Implement the reader functor in your second favorite language
   (the first being Haskell, of course). *)

module Reader (T : T) : Functor = struct
  (* type ('a, 'b) arrow = 'a -> 'b *)
  type 'a t = T.t -> 'a
  (* let compose f g x = g (f x) *)
  let fmap f ra r = f (ra r)
end
