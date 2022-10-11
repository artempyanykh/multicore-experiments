(* Multi-processing *)

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
  type t = { pid : int; out_ch : out_channel }

  let worker_loop in_chan =
    let pid = Unix.getpid () in
    Format.printf "Worker %d start@." pid;
    let rec go n_done =
      let msg : msg = Marshal.from_channel in_chan in
      if is_debug then Format.printf "Worker %d recv'd a task@." pid;
      match msg with
      | Quit ->
          Format.printf "Worker %d done %d tasks@." pid n_done;
          exit 0
      | Calc n ->
          fib n |> ignore;
          go (n_done + n)
    in
    go 0

  let mk () =
    let in_fd, out_fd = Unix.pipe () in
    let pid = Unix.fork () in
    if pid = 0 then (
      Unix.close out_fd;
      let in_ch = Unix.in_channel_of_descr in_fd in
      worker_loop in_ch |> ignore;
      failwith "Unreachable")
    else (
      Unix.close in_fd;
      let out_ch = Unix.out_channel_of_descr out_fd in
      { pid; out_ch })

  let send { pid; out_ch } (msg : msg) =
    Marshal.to_channel out_ch msg [];
    flush out_ch;
    if is_debug then Format.printf "Sent a task to worker %d@." pid

  let wait { pid; _ } = Unix.waitpid [] pid |> ignore
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
