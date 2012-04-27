(*
 *
 * Draw.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

module type MAKEPRINTER =
sig
  type t

  val print_maze : t -> int -> int -> unit
  val print_maze_numbers : t -> int -> int -> unit
  val print_maze_state : t -> int -> int -> unit
end

module MakePrinter (Val : Maze.MAKEMAZE) : MAKEPRINTER
  with
    type t = Val.maze =

struct
  type t = Val.maze

  let print_maze maze width high =
    let print_line x =
      let rec print_l x =
	function
	  | -1	-> ()
	  | y	->
            begin
              let case = Val.get_case_at_pos maze (x, y)
	      in

	      if Val.Elt.statement case 2 = Val.Elt.Wall then
		Printf.printf "_"
	      else
		Printf.printf " ";
              if Val.Elt.statement case 1 = Val.Elt.Wall then
		Printf.printf "|"
	      else
		Printf.printf " ";
              print_l x (y - 1)
            end
      in

      let case = Val.get_case_at_pos maze (x, width - 1)
      in

      if Val.Elt.statement case 3 = Val.Elt.Wall then
	Printf.printf "|"
      else
	Printf.printf " ";
      print_l x (width - 1);
      Printf.printf "\n"
    in

    let rec print_c =
      function
	| -1	-> ()
	| n	->
	  begin
            print_line n;
            print_c (n - 1)
	  end
    in

    let rec print_first_line =
      function
	| -1	-> ()
	| y	->
	  begin
            let case = Val.get_case_at_pos maze (high - 1, y)
	    in

            Printf.printf " ";
            if Val.Elt.statement case 0 = Val.Elt.Wall then
	      Printf.printf "_"
	    else
	      Printf.printf " ";
            print_first_line (y - 1)
	  end
    in

    print_first_line (width - 1);
    Printf.printf "\n";
    print_c (high - 1)
(* End of print_maze *)

  let print_maze_numbers maze width high =
    let print_line x =
      let rec print_l x =
	function
	  | -1	-> ()
	  | y	->
            begin
              let case = Val.get_case_at_pos maze (x, y) in
              Printf.printf "%2d " (Val.Elt.color case);
              print_l x (y - 1)
            end
      in

      print_l x (width - 1);
      Printf.printf "\n"
    in

    let rec print_c =
      function
	| -1	-> ()
	| n	->
	  begin
            print_line n;
            print_c (n - 1)
	  end
    in

    let rec print_first_line =
      function
	| -1	-> ()
	| y	->
	  begin
            let case = Val.get_case_at_pos maze (0, y)
	    in

            Printf.printf "%d " (Val.Elt.color case);
            print_first_line (y - 1)
	  end
    in

    print_c (high - 1);
    Printf.printf "\n"
(* End of print_maze_number *)

  let print_maze_state maze width high =
    let print_line x =
      let rec print_l x =
	function
	  | -1	-> ()
	  | y	->
            begin
              let case = Val.get_case_at_pos maze (x, y)
	      and
		  print_st =
		function
		  | Val.Elt.Wall	-> Printf.printf "Wall "
		  | Val.Elt.Door	-> Printf.printf "Door "
	      in

              Printf.printf "%2d " (Val.Elt.color case);
              print_st (Val.Elt.statement case 0);
	      print_st (Val.Elt.statement case 1);
	      print_st (Val.Elt.statement case 2);
	      print_st (Val.Elt.statement case 3);
              Printf.printf "\n";
              print_l x (y - 1)
            end
      in

      print_l x (width - 1);
    in

    let rec print_c =
      function
	| -1	-> ()
	| n	->
	  begin
            print_line n;
            print_c (n - 1)
	  end
    in

    let rec print_first_line =
      function
	| -1	-> ()
	| y	->
	  begin
            let case = Val.get_case_at_pos maze (0, y)
	    in

            Printf.printf "%d " (Val.Elt.color case);
            print_first_line (y - 1)
	  end
    in

    print_c (high - 1);
    Printf.printf "\n"
(* End of print_maze_state *)

end
