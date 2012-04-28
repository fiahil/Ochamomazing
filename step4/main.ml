(*
 *
 * Made by Fiahil
 *
 *)

let len = ref 0
let hig = ref 0

module SqMaze = Maze.MakeMaze (Case.Hexa)
(* module HeMaze = Maze.MakeMaze (Case.Hexa) *)
module SqPrint = DrawSdl.MakeDraw (SqMaze)
module SqSolve = Pathfinder.MakePathfinder (SqMaze)

let selectDim v =
  if !len = 0 then
    len := int_of_string v
  else
    hig := int_of_string v

let main () =
  let _ = Arg.parse [] (selectDim) "usage: X Y."
  in

  if !len > 0 && !hig > 0 then
    (* let entry = (Random.int !hig, Random.int !len) *)
    let entry = (5, 5)
    and
        out = (8, 2)
    in

    (* SqPrint.print_maze *)
    (* (SqMaze.colorize *)
    (* (SqMaze.create !len !hig) !len !hig) *)
    (* entry !len !hig *)
    let maze = SqMaze.colorize (SqMaze.create !len !hig) !len !hig in

    SqMaze.set_color_at_pos maze (8, 2) 3;
    SqSolve.solve maze entry out;
    SqPrint.print_maze maze entry !len !hig;
    ()
  else
    prerr_endline "Bad arguments. X & Y must be > 0."

let _ = Random.self_init ()
let _ =
  try
    main ()
  with
    | Failure "int_of_string"                   ->
      prerr_endline ("Cannot transform characters into numbers.")
    | Failure "Invalid wall combination"        ->
      prerr_endline "Maze too small."
(* | _                         -> prerr_endline ("An error occured.") *)
