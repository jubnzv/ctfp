(* 5. Define a bifunctor in a language other than Haskell. Implement
   bimap for a generic pair in that language. *)
module type Bifunctor = sig
  type ('a, 'b) t
  val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  (* val first : ('a -> 'b) -> ('a, 'b) t -> ('c, 'b) t  *)
  (* val second : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t *)
end

module Pair : Bifunctor = struct
  type ('a, 'b) t = 'a * 'b
  let bimap f g (l, r) = f l, g r
end
