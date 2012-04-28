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

  module SqNum = Draw.MakePrinter (Val)

  let solve maze entry out =
    print_endline "J'esaye de solve";
    let comp_tuple (f1, s1) (f2, s2) =
      ((f1 = f2) && (s1 = s2))
    in

    let ret_current old current =
      current
    in

    let test_end current =
      function
        | true    -> ret_current (Val.set_color_at_pos maze current 3) (-42, -42)
        | _       -> current
    in

    let stat cur di =
      Val.Elt.statement (Val.get_case_at_pos maze cur) di
    in

    let at_right dir =
      if dir >= (Val.Elt.numberSides - 1) then
        0
      else
        dir + 1
    and
        at_left dir =
      if dir <= 0 then
        Val.Elt.numberSides - 1
      else
        dir - 1
    in

    let color_path old current dir =
      function
        | (0, _)        ->
          (test_end
             (Val.set_color_at_pos maze current 1)
             (comp_tuple current out), at_right dir, stat current (at_right dir))
        | (1, 1)        ->
          (ret_current
             (Val.set_color_at_pos maze old 0)
             current, at_right dir, stat current (at_right dir))
        | (2, 1)        ->
          (ret_current
             (Val.set_color_at_pos maze old 0)
             current, at_right dir, stat current (at_right dir))
        | (x, y)        ->
          failwith ("Impossible color pattern: " ^
                       (string_of_int x) ^
                       ", " ^
                       (string_of_int y))
    in

    let get_color pos =
      Val.Elt.color (Val.get_case_at_pos maze pos)
    in

    let move_path (pos, dir) =
      let value = Val.Elt.get_adj_case pos (Val.Elt.get_dir_pattern dir)
      in

      color_path pos value dir (get_color value, get_color pos)
    in

    let rec in_find =
      function
        | ((-42, -42), _, _)                      -> maze
        | (current, dir, Val.Elt.Door)            ->
	  begin
	    Printf.printf "Deplacement: %d, %d -- %d\n" (fst current) (snd current) dir;
	    SqNum.print_maze_numbers maze 10 10;
            in_find (move_path (current, dir))
	  end
        | (current, dir, _)                       ->
	  begin
	    Printf.printf "Rotation:    %d, %d -- %d\n" (fst current) (snd current) dir;
	    SqNum.print_maze_numbers maze 10 10;
            in_find (current, at_left dir, stat current (at_left dir))
	  end
    in

    if comp_tuple entry out then
      maze
    else
      in_find (entry, 0,
               (Val.Elt.statement
                  (Val.get_case_at_pos maze
                     (Val.set_color_at_pos maze entry 2)) 0));
    print_endline "J'ai fait de la merde";
    maze
end
