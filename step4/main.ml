(*
 *
 * Made by Fiahil
 *
 *)

let len = ref 0
let hig = ref 0
let mode= ref 0

module SqMaze  = Maze.MakeMaze (Case.Square)
module SqPrint = DrawSdl.MakeDraw (SqMaze)
module SqSolve = Pathfinder.MakePathfinder (SqMaze)

module HeMaze  = Maze.MakeMaze (Case.Hexa)
module HePrint = DrawSdl.MakeDraw (HeMaze)
module HeSolve = Pathfinder.MakePathfinder (HeMaze)

let selectDim v =
  if !len = 0 then
    len := int_of_string v
  else
    hig := int_of_string v

let selectHexa () =
  mode := 1

let selectSquare () =
  mode := 2

let main () =
  let _ = Arg.parse
    [("--square", Arg.Unit selectSquare, "Select Square mode");
     ("--hexagon", Arg.Unit selectHexa, "Select Hexagonal mode")]
    (selectDim) "usage: X Y <--square | --hexagon>."
  in

  if !len > 0 && !hig > 0 && !mode > 0 then
    let entry = (Random.int !hig, Random.int !len)
    and
        out = (Random.int !hig, Random.int !len)
    in

    if !mode = 2 then
      SqPrint.print_maze
        (SqSolve.solve
           (SqMaze.colorize
              (SqMaze.create !len !hig) !len !hig) entry out) entry !len !hig
    else
      HePrint.print_maze
        (HeSolve.solve
           (HeMaze.colorize
              (HeMaze.create !len !hig) !len !hig) entry out) entry !len !hig
  else if !mode = 0 then
    prerr_endline "Bad arguments. No mode selected."
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
    | Exit					->
      ()
    | _                                         ->
      prerr_endline ("An error occured.")
