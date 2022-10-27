(* Multi-threading with domains *)

open Domainslib

let is_debug = false
let progname = Sys.argv.(0)
let volume = try int_of_string Sys.argv.(1) with _ -> 42

(* Generate a list that looks like this for n = 4: [1; 1; 1; 1; 2; 2; 2; 3; 3; 4] *)
let tasks =
  List.init volume (fun i ->
      let n = volume - i in
      List.init n (fun _ -> i + 1))
  |> List.flatten |> Array.of_list

type msg = Calc of int | Quit

let rec fib_aux acc n =
  if n < 2 then acc + 1
  else fib_aux (fib_aux acc (n-2)) (n-1)

let fib n = fib_aux 0 n

module Worker = struct
  type 'a t = { dom : 'a Domain.t; chan : msg Chan.t }

  let worker_loop chan () =
    let pid = (Domain.self () :> int) in
    Format.printf "Worker %d start@." pid;
    let out_name = Format.sprintf "%s-%d.txt" progname pid in
    let out_chan = open_out out_name in
    let rec go n_done =
      let msg = Chan.recv chan in
      if is_debug then Format.printf "Worker %d recv'd a task@." pid;
      match msg with
      | Quit ->
          flush out_chan;
          close_out out_chan;
          Format.printf "Worker %d done %d tasks@." pid n_done;
          ()
      | Calc n ->
          fib n |> Printf.fprintf out_chan "%d\n";
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
