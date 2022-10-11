(* Multi-threading with domains *)

open Domainslib

let is_debug = false
let volume = try int_of_string Sys.argv.(1) with _ -> 42

(* Generate a list that looks like this for n = 4: [1; 1; 1; 1; 2; 2; 2; 3; 3; 4] *)
let tasks =
  List.init volume (fun i ->
      let n = volume - i in
      List.init n (fun _ -> i + 1))
  |> List.flatten |> Array.of_list

type msg = Calc of int | Quit

let rec fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2)

module Worker = struct
  type 'a t = { dom : 'a Domain.t; chan : msg Chan.t }

  let worker_loop chan () =
    let pid = (Domain.self () :> int) in
    Format.printf "Worker %d start@." pid;
    let rec go n_done =
      let msg = Chan.recv chan in
      if is_debug then Format.printf "Worker %d recv'd a task@." pid;
      match msg with
      | Quit ->
          Format.printf "Worker %d done %d tasks@." pid n_done;
          ()
      | Calc n ->
          fib n |> ignore;
          go (n_done + n)
    in
    go 0

  let mk () =
    let chan = Chan.make_unbounded () in
    let dom = Domain.spawn (worker_loop chan) in
    { dom; chan }

  let send { chan; _ } msg = Chan.send chan msg
  let wait { dom; _ } = Domain.join dom |> ignore
end

let () =
  let n_workers = 4 in
  let workers = Array.init n_workers (fun _ -> Worker.mk ()) in
  let seed = 42 in
  Random.init seed;
  Array.iter
    (fun t ->
      let wn = Random.int n_workers in
      let w = workers.(wn) in
      Worker.send w (Calc t))
    tasks;
  Array.iter (fun w -> Worker.send w Quit) workers;
  Array.iter (fun w -> Worker.wait w) workers;
  Format.printf "Main done@.";
  ()
