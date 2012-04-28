(*
 *
 * Made by Fiahil
 *
 *)

let len = ref 0
let hig = ref 0

module SqMaze = Maze.MakeMaze (Case.Square)
(* module HeMaze = Maze.MakeMaze (Case.Hexa) *)
module SqPrint = DrawSdl.MakeDraw (SqMaze)

let selectDim v =
  if !len = 0 then
    len := int_of_string v
  else
    hig := int_of_string v

let main () =
  let _ = Arg.parse [] (selectDim) "usage: X Y."
  in

  if !len > 0 && !hig > 0 then
    begin
      let maze = SqMaze.colorize (SqMaze.create !len !hig) !len !hig
      in

      (* SqPrint.print_maze maze !len !hig; *)
      SqPrint.print_maze maze (0, 0) !len !hig
    end
  else
    prerr_endline "Bad arguments. X & Y must be > 0."

let _ = Random.self_init ()
let _ =
  try
    main ()
  with
    | Failure "int_of_string"			->
      prerr_endline ("Cannot transform characters into numbers.")
    | Failure "Invalid wall combination"	->
      prerr_endline "Maze too small."
    | _                         -> prerr_endline ("An error occured.")
