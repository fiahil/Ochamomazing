(*
 *
 * Maze.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

module type MAKEMAZE =
sig
  type maze

  module Elt : Case.CASE

  val create : int -> int -> maze
  val colorize : maze -> int -> int -> maze
  val get_case_at_pos : maze -> int * int -> Elt.case
  val set_color_at_pos : maze -> int * int -> int -> int * int
end

module MakeMaze (Val : Case.CASE) : MAKEMAZE
  with
    type maze = Val.case array array =

struct
  type maze = Val.case array array

  module Elt = Val

  let picker = ref []

  let create width high =
    let rec cur = ref 0
    and
	next = ref 0
    and
	create_line line value =
      function
	| -1	-> line
	| n	->
	  begin
            Array.set line n (Val.create value);
	    picker := (!cur, n)::!picker;
	    next := !next + 1;
            create_line line (value + 1) (n - 1)
	  end
    in

    let rec create_map map width =
      function
	| -1	-> map
	| n	->
	  begin
            Array.set map n
	      (create_line
		 (Array.make width (Val.create (n * width)))
		 (n * width)
		 (width - 1));
	    cur := !cur + 1;
            create_map map width (n - 1)
	  end
    and
	printer (a, b) = Printf.printf "%d - %d\n" a b
    in

    begin
      let pp = create_map (Array.make high [||]) width (high - 1);
      in

      List.iter printer !picker;
      pp(* TODO*)
    end

  let get_case_at_pos maze (x, y) =
    Array.get (Array.get maze x) y

  let set_color_at_pos maze (x, y) col =
    Array.set maze.(x) y (Val.set_color (get_case_at_pos maze (x, y)) col);
    (x, y)

  let colorize maze width high =
    let inverse_tuple (x, y) = (-x, -y)
    in

    let get_rand_case () =
      let ret =
	if List.length !picker > 0 then
	  List.hd !picker
	else
	  (Random.int width, Random.int high)
      in

      begin
	Printf.printf "%d # %d\n" (fst ret) (snd ret);
	if List.length !picker > 0 then
	  picker := (List.tl !picker)
	else
	  ();
	ret
      end
    in

    let get_rand_dir () = Val.get_dir_pattern (Random.int Elt.numberSides)
    in

    let check_position (x, y) =
      x > -1 && y > -1 && x < high && y < width
    in

    let add_tuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    in

    let change_case_by_color color new_color =
      let rec change_columns x =
	function
          | -1	-> ()
          | y	->
            if (Val.color (get_case_at_pos maze (x, y)) = color)
            then
	      begin
		Array.set maze.(x) y
		  (Val.set_color (get_case_at_pos maze (x, y)) new_color);
		change_columns x (y - 1)
	      end
            else
	      change_columns x (y - 1)
      in

      let rec change_lines =
	function
          | -1	-> ()
          | x	->
            begin
	      change_columns x (width - 1);
	      change_lines (x - 1)
            end
      in

      change_lines (high - 1)
    in

    let change_case case dir =
      let change_wall case dir = Val.set_dir_pattern case dir
      in


      if (check_position (add_tuple dir case) &&
	    (Val.color (get_case_at_pos maze case) !=
		Val.color (get_case_at_pos maze (add_tuple dir case))))
      then
	begin
          Array.set maze.(fst (add_tuple dir case)) (snd (add_tuple dir case))
	    (change_wall
	       (get_case_at_pos maze (add_tuple dir case))
	       (inverse_tuple dir));
          Array.set maze.(fst case) (snd case)
	    (change_wall
	       (get_case_at_pos maze case) dir);
          change_case_by_color
	    (max (Val.color (get_case_at_pos maze (add_tuple dir case)))
	       (Val.color (get_case_at_pos maze case)))
	    (min (Val.color (get_case_at_pos maze (add_tuple dir case)))
	       (Val.color (get_case_at_pos maze case)));
          1
	end
      else
	0
    in

    let rec change_cases =
      function
	| 0	-> ()
	| n	-> change_cases (n - (change_case (get_rand_case()) (get_rand_dir())))
    in

    change_cases (width * high - 1);
    maze
end
