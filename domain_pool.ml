(* Multi-threading with task pool *)

open Domainslib

let volume = try int_of_string Sys.argv.(1) with _ -> 42

(* Generate a list that looks like this for n = 4: [1; 1; 1; 1; 2; 2; 2; 3; 3; 4] *)
let tasks =
  List.init volume (fun i ->
      let n = volume - i in
      List.init n (fun _ -> i + 1))
  |> List.flatten |> Array.of_list

let rec fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2)

let () =
  let n_workers = 4 in
  let n_done = Array.make n_workers 0 in
  let pool = Task.setup_pool ~num_domains:(n_workers - 1) () in
  Format.printf "Main start@.";
  Task.run pool (fun _ ->
      Task.parallel_for pool ~start:0
        ~finish:(Array.length tasks - 1)
        ~body:(fun idx ->
          let id = (Domain.self () :> int) in
          let n = tasks.(idx) in
          fib n |> ignore;
          n_done.(id) <- n_done.(id) + n));
  Task.teardown_pool pool;
  Array.iteri (fun i n -> Format.printf "Worker %d done %d tasks@." i n) n_done;
  Format.printf "Main done@.";
  ()
