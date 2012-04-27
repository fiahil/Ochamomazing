(*
 *
 * Draw.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

let print_maze maze width high =
  let print_line x =
    let rec print_l x =
      function
	| -1	-> ()
	| y	->
          begin
            let case = Maze.get_case_at_pos maze (x, y)
	    in

	    if Case.statement case 2 = Case.Wall then
	      Printf.printf "_"
	    else
	      Printf.printf " ";
            if Case.statement case 1 = Case.Wall then
	      Printf.printf "|"
	    else
	      Printf.printf " ";
            print_l x (y - 1)
          end
    in

    let case = Maze.get_case_at_pos maze (x, width - 1)
    in

    if Case.statement case 3 = Case.Wall then
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
          let case = Maze.get_case_at_pos maze (high - 1, y)
	  in

          Printf.printf " ";
          if Case.statement case 0 = Case.Wall then
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
            let case = Maze.get_case_at_pos maze (x, y) in
	    if (Case.color case = 1) then
              Printf.printf "[31;41m # [00m"
	    else if (Case.color case = 2) then
              Printf.printf "[32;42m # [00m"
	    else if (Case.color case = 3) then
              Printf.printf "[33;43m # [00m"
	    else
	      Printf.printf "%2d " (Case.color case);
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
          let case = Maze.get_case_at_pos maze (0, y)
	  in

          Printf.printf "%d " (Case.color case);
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
            let case = Maze.get_case_at_pos maze (x, y)
	    and
		print_st =
	      function
		| Case.Wall	-> Printf.printf "Wall "
		| Case.Door	-> Printf.printf "Door "
	    in

            Printf.printf "por x: %2d y: %2d color %2d " x y (Case.color case);
	    Printf.printf " 0 = ";
            print_st (Case.statement case 0);
	    Printf.printf " | 1 = ";
	    print_st (Case.statement case 1);
	    Printf.printf " | 2 = ";
	    print_st (Case.statement case 2);
	    Printf.printf " | 3 = ";
	    print_st (Case.statement case 3);
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
          let case = Maze.get_case_at_pos maze (0, y)
	  in

          Printf.printf "%d " (Case.color case);
          print_first_line (y - 1)
	end
  in

  print_c (high - 1);
  Printf.printf "\n"

(* End of print_maze_state *)
