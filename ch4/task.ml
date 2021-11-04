(* 1. Construct the Kleisli category for partial functions (define com-
   position and identity). *)
module Kleisli_Partial = struct
  type a
  type b
  type c

  let identity : 'a -> 'a option = fun a -> Some(a)

  let ( >=> ) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option =
    fun m1 m2 x ->
    match m1 x with
    | Some(y) -> m2 y
    | None -> None
end

module Safe = struct
  let safe_root : float -> float option =
    fun x -> if x >= 0. then Float.sqrt x |> Option.some else None

  (* 2. Implement the embellished function safe_reciprocal that re-
        turns a valid reciprocal of its argument, if it’s different from zero. *)
  let safe_reciprocal : float -> float option =
    fun x -> if Float.compare x 0. != 0 then 1. /. x |> Option.some else None

  (* 3. Compose the functions safe_root and safe_reciprocal to
     implement safe_root_reciprocal that calculates sqrt(1/x)
     whenever possible. *)
  let safe_root_reciprocal : float -> float option =
    Kleisli_Partial.( >=> ) safe_root safe_reciprocal
end

let () =
  let test x =
    match Safe.safe_root_reciprocal x with
    | Some(res) -> Printf.printf "%f: %.3f\n" x res
    | None -> Printf.printf "%f: unknown\n" x
  in
  test 3.; (* 0.577 *)
  test 0.  (* unknown *)
