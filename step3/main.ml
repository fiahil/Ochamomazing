(*
 *
 * Made by Fiahil
 *
 *)

let len = ref 0
let hig = ref 0

let selectDim v =
  if !len = 0 then
    len := int_of_string v
  else
    hig := int_of_string v

let main () =
  let _ = Arg.parse [] (selectDim) "usage: X Y"
  in

  if !len > 0 && !hig > 0 then
    let entry = (Random.int !hig, Random.int !len)
    and
	out = (Random.int !hig, Random.int !len)
    in

    DrawSdl.print_maze
      (Pathfinder.solve
	 (Maze.colorize
	    (Maze.create !len !hig) !len !hig)
	 entry out) entry !len !hig
  else
    prerr_endline "Bad arguments. X & Y must be > 0."

let _ = Random.self_init ()
let _ =
  try
    main ()
  with
    | Failure "int_of_string"   ->
      prerr_endline ("Cannot transform characters into numbers.")
    | Failure "Invalid wall combination"	->
      prerr_endline "Maze too small."
    | _				-> prerr_endline ("An error occured.")
