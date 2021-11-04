(* Compilation: ocamlc unix.cma task.ml *)

(* 1. Define a higher-order function (or a function object) memoize in
   your favorite language. This function takes a pure function f as
   an argument and returns a function that behaves almost exactly
   like f, except that it only calls the original function once for every
   argument, stores the result internally, and subsequently returns
   this stored result every time it’s called with the same argument.
   You can tell the memoized function from the original by watch-
   ing its performance. For instance, try to memoize a function that
   takes a long time to evaluate. You’ll have to wait for the result
   the first time you call it, but on subsequent calls, with the same
   argument, you should get the result immediately. *)
module M = struct
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  let memo = ref (IntMap.empty)
  let memoize (f: int -> int) =
    fun (a: int) -> match IntMap.find_opt a (!memo) with
      | Some(v) -> v
      | None -> let v = f a in memo := IntMap.add a v (!memo); v
end

let test_task_1 () =
  Printf.printf "Testing task 1 ... ";
  let calc = fun a -> Unix.sleepf 0.01; a + 1
  and sum = ref 0 in
  for i = 1 to 20 do
    sum := !sum + (M.memoize calc i);
    (* sum := !sum + (calc i); *)
    for j = 20 downto 1 do
      sum := !sum + (M.memoize calc j);
      (* sum := !sum + (calc j); *)
    done
  done;
  (* Printf.printf "sum=%d\n" !sum *)
  Printf.printf "OK\n"

(* 2. Try to memoize a function from your standard library that you normally
      use to produce random numbers. Does it work? *)
let test_task_2 () =
  Printf.printf "Testing task 2 ... ";
  let r1 = M.memoize Random.int 100
  and r2 = M.memoize Random.int 100
  in
  (* It returns the same result, because memoize keeps result for 100. *)
  assert (r1 == r2);
  Printf.printf "OK\n"

(* 3. Most random number generators can be initialized with a seed. Implement a
   function that takes a seed, calls the random number generator with that
   seed, and returns the result. Memoize that function. Does it work? *)
let test_task_3 () =
  Printf.printf "Testing task 3 ... ";
  let r seed = Random.init seed; Random.int 100 in
  let r1 = M.memoize r 42
  and r2 = M.memoize r 43
  in
  (* This, obviously, works as expected. *)
  assert (r1 != r2);
  Printf.printf "OK\n"

(* 4. Which of these C++ functions are pure? Try to memoize them
   and observe what happens when you call them multiple times: memoized and
   not.
   (a) The factorial function from the example in the text.
   (b) std::getchar
   (b) bool f () {
     std::cout << "Hello!" << std::endl;
     return true;
   }
   (d) int f(int x) {
     static int y = 0;
     y += x;
     return y;
   } *)

(* Only (a) is pure. *)

(* 5. How many different functions are there from Bool to Bool? Can you
   implement them all? *)
let bool_id (b : bool) = b
let bool_neg (b : bool) = not b
let bool_false (_ : bool) = false
let bool_true (_ : bool) = true

(* 6. Draw a picture of a category whose only objects are the types
   Void, () (unit), and Bool; with arrows corresponding to all possible functions
   between these types. Label the arrows with the names of the functions. *)

let () =
  test_task_1 ();
  test_task_2 ();
  test_task_3 ()
