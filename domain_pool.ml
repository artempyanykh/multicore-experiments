(* Multi-threading with task pool *)

open Domainslib

let () =
  let n_workers = 4 in
  let pool = Task.setup_pool ~num_additional_domains:(n_workers - 1) () in
  Task.parallel_for pool ~start:0 ~finish:5 ~body:(fun _idx -> ());
  Task.teardown_pool pool;
  Printf.printf "Main done";
  ()
