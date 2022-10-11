let cell = ref 0
let latch = ref 0

let work () =
  let id = (Domain.self () :> int) in
  while !latch = 0 do
    Domain.cpu_relax ()
  done;
  cell := id;
  ()

let () =
  let d1, d2 = Domain.spawn work, Domain.spawn work in
  latch := 1;
  [d1; d2] |> List.iter (fun d -> Domain.join d);
  Format.printf "Final cell value = %d@." !cell;
  ()
