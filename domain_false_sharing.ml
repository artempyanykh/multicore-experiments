let worker_loop n padding shared n_iters () =
  let id = (Domain.self () :> int) in
  let cell = n * padding in
  Format.printf "Worker %d started@." id;
  for _i = 0 to n_iters - 1 do
    shared.(cell) <- shared.(cell) + 1
  done;
  Format.printf "Worker %d done: %d@." id shared.(cell);
  ()

let () =
  let n_workers = 4 in
  let padding = 128 in
  let n_iters = 50_000_000 in
  let shared = Array.make (n_workers * padding) 0 in
  let domains =
    Array.init n_workers (fun n ->
        Domain.spawn (worker_loop n padding shared n_iters))
  in
  Array.iter (fun d -> Domain.join d) domains;
  Format.printf "Main done@.";
  ()
