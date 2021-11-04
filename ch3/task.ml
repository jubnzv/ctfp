(* 1. Generate a free category from:
   (a) A graph with one node and no edges
   (b) A graph with one node and one (directed) edge (hint: this
   edge can be composed with itself)
   (c) A graph with two nodes and a single arrow between them
   (d) A graph with a single node and 26 arrows marked with the
   letters of the alphabet: a, b, c … z. *)

(* 2. What kind of order is this?
   (a) A set of sets with the inclusion relation: 𝐴 is included in 𝐵 if every
   element of 𝐴 is also an element of 𝐵.
   (b) C++ types with the following subtyping relation: T1 is a subtype of T2
   if a pointer to T1 can be passed to a function that expects a pointer to T2
   without triggering a compilation error. *)

(* (a) partial order *)
(* (b) partial order *)

(* 3. Considering that Bool is a set of two values True and False, show
   that it forms two (set-theoretical) monoids with respect to, respectively,
   operator && (AND) and || (OR). *)
module type Monoid = sig
  type t
  val id : t
  val ( <> ) : t -> t -> t
end

module AndMonoid : (Monoid with type t = bool) = struct
  type t = bool
  let id = false
  let ( <> ) = ( && )
end

module OrMonoid : (Monoid with type t = bool) = struct
  type t = bool
  let id = false
  let ( <> ) = ( || )
end

(*4. Represent the Bool monoid with the AND operator as a category: List the
     morphisms and their rules of composition. *)

(* 5. Represent addition modulo 3 as a monoid category. *)
