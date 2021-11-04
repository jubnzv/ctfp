(* 1. Implement, as best as you can, the identity function in your fa-
   vorite language (or the second favorite, if your favorite language
   happens to be Haskell). *)
let id: 'a -> 'a = fun id -> id

(* 2. Implement the composition function in your favorite language. It
   takes two functions as arguments and returns a function that is
   their composition. *)
let compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun f g x -> g (f x)

(* 3. Write a program that tries to test that your composition function
      respects identity. *)
let () =
  let id x = x in
  assert(compose id id 4 == 4);
  assert(compose (( * ) 7) id 4 == 28);
  assert(compose id (( * ) 7) 4 == 28)

(* 4. Is the world-wide web a category in any sense? Are links morphisms? *)
(* Yes, if they have identities. *)

(* 5. Is Facebook a category, with people as objects and friendships as morphisms? *)
(* No, see 4. *)

(* 6. When is a directed graph a category? *)
(* When each node have an indentity arrow. *)
