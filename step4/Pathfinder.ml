(*
 *
 * Pathfinder.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

module type MAKEPATHFINDER =
sig
  type t

  val solve : t -> (int * int) -> (int * int) -> t
end

module MakePathfinder (Val : Maze.MAKEMAZE) : MAKEPATHFINDER
  with
    type t = Val.maze =

struct
  type t = Val.maze

  let solve maze entry out =
    let comp_tuple (f1, s1) (f2, s2) =
      ((f1 = f2) && (s1 = s2))
    in

    let ret_current old current =
      current
    in

    let test_end current =
      function
	| true	-> ret_current (Val.set_color_at_pos maze current 3) (-42, -42)
	| _	-> current
    in

    let stat cur di =
      Val.Elt.statement (Val.get_case_at_pos maze cur) di
    in

    let at_right =
      function
	| 0	-> 1
	| 1	-> 2
	| 2	-> 3
	| 3	-> 0
	| _	-> failwith "Impossible direction."
    in

    let color_path old current dir =
      function
	| (0, _)	->
	  (test_end
	     (Val.set_color_at_pos maze current 1)
	     (comp_tuple current out), at_right dir, stat current (at_right dir))
	| (1, 1)	->
	  (ret_current
	     (Val.set_color_at_pos maze old 0)
	     current, at_right dir, stat current (at_right dir))
	| (2, 1)	->
	  (ret_current
	     (Val.set_color_at_pos maze old 0)
	     current, at_right dir, stat current (at_right dir))
	| _	-> failwith "Impossible color pattern."
    in

    let get_color pos =
      Val.Elt.color (Val.get_case_at_pos maze pos)
    in

    let move_path =
      function
	| ((cx, cy), 0)	->
	  color_path (cx, cy) (cx + 1, cy) 0 (get_color (cx + 1, cy), get_color (cx, cy))
	| ((cx, cy), 1)	->
	  color_path (cx, cy) (cx, cy - 1) 1 (get_color (cx, cy - 1), get_color (cx, cy))
	| ((cx, cy), 2)	->
	  color_path (cx, cy) (cx - 1, cy) 2 (get_color (cx - 1, cy), get_color (cx, cy))
	| ((cx, cy), 3)	->
	  color_path (cx, cy) (cx, cy + 1) 3 (get_color (cx, cy + 1), get_color (cx, cy))
	| _		-> failwith "Execution fatal error."
    in

    let rec in_find =
      function
	| ((-42, -42), _, _)		-> maze
	| (current, dir, Val.Elt.Door)	->
	  in_find (move_path (current, dir))
	| (current, 0, _)			->
	  in_find (current, 3, stat current 3)
	| (current, 1, _)			->
	  in_find (current, 0, stat current 0)
	| (current, 2, _)			->
	  in_find (current, 1, stat current 1)
	| (current, 3, _)			->
	  in_find (current, 2, stat current 2)
	| _				-> failwith "Execution fatal error"
    in

    if (comp_tuple entry out) then
      maze
    else
      in_find (entry, 0,
	       (Val.Elt.statement
		  (Val.get_case_at_pos maze
		     (Val.set_color_at_pos maze entry 2)) 0))
end
