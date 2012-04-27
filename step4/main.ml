(*
 *
 * Made by Fiahil
 *
 *)

let len = ref 0
let hig = ref 0

module SqMaze = Maze.MakeMaze (Case.Case)
module SqPrint = Draw.MakePrinter (SqMaze)

let selectDim v =
  if !len = 0 then
    len := int_of_string v
  else
    hig := int_of_string v

let main () =
  let _ = Arg.parse [] (selectDim) "usage: X Y"
  in

  if !len > 0 && !hig > 0 then
    begin
      let maze = SqMaze.colorize (SqMaze.create !len !hig) !len !hig
      in

      SqPrint.print_maze maze !len !hig;
      SqPrint.print_maze_numbers maze !len !hig
    end
  else
    prerr_endline "Bad arguments. X & Y must be > 0."

let _ = Random.self_init ()
let _ =
  try
    main ()
  with
    | Failure "int_of_string"   ->
      prerr_endline ("Cannot transform characters into numbers.")
    | _                         -> prerr_endline ("An error occured.")
