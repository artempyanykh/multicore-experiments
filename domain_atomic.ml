let cell = Atomic.make (-1)
let n_wins = Array.make 2 0
let latch = Atomic.make 0

let work n () =
  while Atomic.get latch = 0 do
    () (* Domain.cpu_relax () *)
  done;
  Atomic.compare_and_set cell (-1) n |> ignore;
  ()

let () =
  for _iter = 0 to 1_000 do
    Atomic.set cell (-1);
    Atomic.set latch 0;
    let d1, d2 = (Domain.spawn (work 0), Domain.spawn (work 1)) in
    Atomic.set latch 1;
    [ d1; d2 ] |> List.iter (fun d -> Domain.join d);
    let winner = Atomic.get cell in
    n_wins.(winner) <- n_wins.(winner) + 1
  done;
  Format.printf "Wins: 0=%d; 1=%d@." n_wins.(0) n_wins.(1);
  ()
