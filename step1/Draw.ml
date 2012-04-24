(*
 *
 * Draw.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

(* Maze.get_case_at_pos : int -> int -> case *)

let print_maze maze hight width =
  let in_extrem = function
    | Case.Wall -> "===="
    | Case.Door -> "|  |"
  in
  let in_middle = function
    | (Case.Wall, Case.Door) -> "|   "
    | (Case.Wall, Case.Wall) -> "|  |"
    | (Case.Door, Case.Wall) -> "   |"
    | (Case.Door, Case.Door) -> "    "
  in
  let in_concat c (top, middle, back) = function
    | 0 ->
      (
	top ^ (in_extrem (Case.statement c 0)),
	middle ^ (in_middle ((Case.statement c 3), (Case.statement c 2))),
	back ^ (in_extrem (Case.statement c 1))
      )
    | _ ->
      (
	top,
	middle ^ (in_middle ((Case.statement c 3), (Case.statement c 2))),
	back ^ (in_extrem (Case.statement c 1))
      )
  in
  let rec print_line maze width col row (top, middle, back) =
    function
      | false ->
	print_line maze width (col + 1) row
          (in_concat (Maze.get_case_at_pos maze (col, row)) (top, middle, back) row)
          (width = (col + 1))
      | _     ->
        begin
	  if (row = 0) then
	    print_endline top;
          print_endline middle;
          print_endline back
        end
  in
  let rec in_print_maze maze hight width row =
    function
      | false ->
        begin
          print_line maze width 0 row ("", "", "") (width = 0);
          in_print_maze maze hight width (row + 1) (hight = (row + 1))
        end
      | _     -> ()
  in
  in_print_maze maze hight width 0 (hight = 0)
;;
