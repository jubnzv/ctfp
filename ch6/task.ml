type 'a maybe = Nothing | Just of 'a
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) pair = 'a * 'b

(* 1. Show the isomorphism between Maybe a and Either () a. *)
let maybe_to_either : ('a maybe) -> (('a, unit) either) =
  fun mb -> match mb with
    | Just x -> Left(x)
    | Nothing -> Right(())
let either_to_maybe : (('a, unit) either) -> ('a maybe) =
  fun e -> match e with
    | Left x -> Just(x)
    | Right _ -> Nothing

(* 2. Here’s a sum type defined in Haskell:
   data Shape = Circle Float
               | Rect Float Float
   When we want to define a function like area that acts on a Shape, we do it by
   pattern matching on the two constructors:
   area :: Shape -> Float
   area (Circle r) = pi * r * r
   area (Rect d h) = d * h
   Implement Shape in C++ or Java as an interface and create two classes: Circle
   and Rect. Implement area as a virtual function.

   3. Continuing with the previous example: We can easily add a new function circ
   that calculates the circumference of a Shape. We can do it without touching the
   definition of Shape:
   circ :: Shape -> Float
   circ (Circle r) = 2.0 * pi * r
   circ (Rect d h) = 2.0 * (d + h)
   Add circ to your C++ or Java implementation. What parts of the original code
   did you have to touch?

   4. Continuing further: Add a new shape, Square, to Shape and make all the
   necessary updates. What code did you have to touch in Haskell vs. C++ or
   Java? *)

(* This seems like simple task to introduce the expression problem.
   Below is the OOP implementation in OCaml. *)

class virtual shape = object
  method virtual area: float
  method virtual circ: float
end

class circle r = object
  inherit shape
  val mutable r: float = r
  method area = Float.pi *. r *. r
  method circ = 2.0 *. Float.pi *. r
end

class rect d h = object
  inherit shape
  val mutable d: float = d
  val mutable h: float = h
  method area = d *. h
  method circ = 2.0 *. (d +. h)
end

class square x = object
  inherit shape
  val x: float = x
  method area = x *. x
  method circ = 2.0 *. (x +. x)
end

(* 5. Show that 𝑎 + 𝑎 = 2 × 𝑎 holds for types (up to isomorphism). Remember that
      2 corresponds to Bool, according to our translation table. *)
let pair_to_either : ((bool, 'a) pair -> ('a, 'a) either) =
  fun p -> match p with
    | (true, a) -> Left a
    | (false, a) -> Right a

let either_to_pair : (('a, 'a) either -> (bool, 'a) pair) =
  fun e -> match e with
    | Left a -> (true, a)
    | Right a -> (false, a)
