open Domainslib

let volume = 45

let tasks =
  List.init volume (fun i ->
      let n = volume - i in
      List.init n (fun _ -> i + 1))
  |> List.flatten

let take ~n list =
  let rec go n rev_acc rest =
    if n <= 0 then List.rev rev_acc
    else match rest with
         | [] -> List.rev rev_acc
         | h :: tail -> go (n - 1) (h :: rev_acc) tail
  in
  go n [] list

type msg = Calc of int | Quit

let rec fib n =
  if n <= 2 then 1 else (fib (n - 1)) + (fib (n - 2))

let is_debug = false

let worker_loop chan () =
  let pid = (Domain.self () :> int) in
  Format.printf "Worker %d start@." pid;
  let rec go () =
    let msg = Chan.recv chan in
    if is_debug then Format.printf "Worker %d recv'd a task@." pid;
    ( match msg with
      | Quit ->
         Format.printf "Worker %d done@." pid;
         ()
      | Calc n ->
         fib n |> ignore;
         go ()
    )
  in go ()

let () =
  let n_workers = 4 in
  let chan = Chan.make_unbounded () in
  let workers = List.init n_workers (fun _ -> Domain.spawn (worker_loop chan)) |> Array.of_list in
  List.iter (fun t -> Chan.send chan (Calc t)) tasks;
  List.init n_workers (fun _ -> Chan.send chan Quit) |> ignore;
  Array.iter (fun w -> Domain.join w) workers;
  Printf.printf "Main done";
  ()
