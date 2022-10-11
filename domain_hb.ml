let msg = ref 0
let latch = ref false

let work1 () =
  msg := 1;
  latch := true;
  ()

type result = Good | Broken

let work2 () =
  let latch_v = !latch in
  let msg_v = !msg in
  if latch_v = true && msg_v = 0 then
    Broken
  else
    Good

let () =
  let repeat = ref true in
  let iter = ref 0 in
  while !repeat && !iter < 10000 do
    msg := 0;
    latch := false;
    let d1, d2 = Domain.spawn work1, Domain.spawn work2 in
    Domain.join d1 |> ignore;
    let r = Domain.join d2 in
    (match r with
    | Good -> repeat := true
    | Broken ->
       repeat := false;
       Printf.printf "Broken");
    iter := !iter + 1
  done
