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

module Worker = struct
  type t = {pid: int; out_ch: out_channel}

  let worker_loop in_chan =
    let pid = Unix.getpid () in
    Format.printf "Worker %d start@." pid;
    let rec go () =
      let msg: msg = Marshal.from_channel in_chan in
      if is_debug then Format.printf "Worker %d recv'd a task@." pid;
      ( match msg with
        | Quit ->
           Format.printf "Worker %d done@." (Unix.getpid ());
           exit 0
        | Calc n ->
           fib n |> ignore;
           go ()
      )
    in go ()

  let mk () =
    let in_fd, out_fd = Unix.pipe () in
    let pid = Unix.fork () in
    if pid = 0 then
      ( Unix.close out_fd;
        let in_ch = Unix.in_channel_of_descr in_fd in
        worker_loop in_ch |> ignore;
        failwith "Unreachable")
    else
      ( Unix.close in_fd;
        let out_ch = Unix.out_channel_of_descr out_fd in
        {pid; out_ch} )

  let send (msg: msg) {pid; out_ch} =
    Marshal.to_channel out_ch msg [];
    flush out_ch;
    if is_debug then Format.printf "Sent a task to worker %d@." pid

  let wait {pid} =
    Unix.waitpid [] pid |> ignore
end

let () =
  let n_workers = 4 in
  let workers = List.init n_workers (fun _ -> Worker.mk ()) |> Array.of_list in
  List.iter (fun t ->
      let wn = Random.int n_workers in
      let w = workers.(wn) in
      Worker.send (Calc t) w
    ) tasks;
  Array.iter (fun w -> Worker.send Quit w) workers;
  Array.iter (fun w -> Worker.wait w) workers;
  Printf.printf "Main done";
  ()
