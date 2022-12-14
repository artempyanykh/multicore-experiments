let cell = ref (-1)
let n_wins = Array.make 2 0

let work n () =
  cell := n;
  ()

let () =
  for _iter = 0 to 1_000 do
    cell := -1;
    let d1, d2 = (Domain.spawn (work 0), Domain.spawn (work 1)) in
    [ d1; d2 ] |> List.iter (fun d -> Domain.join d);
    let winner = !cell in
    n_wins.(winner) <- n_wins.(winner) + 1
  done;
  Format.printf "Wins: 0=%d; 1=%d@." n_wins.(0) n_wins.(1);
  ()
