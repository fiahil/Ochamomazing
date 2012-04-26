(*
 *
 * Pathfinder.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

(**

   Parcours :

- on laisse une trainee de 1 sur la ou on passe
- on passe par la premiere porte dispo en partant de la droite
- on cheque que la case derriere la porte
 (existe ! Edit : pas de porte sur zone vide donc pas de reel problem) et ne soit pas deja a 1
- si on a plus de solution on backpedale jusqu'a un branchement avec une autre solution
- ainsi de suite jusqua solution

   Pb :

- si 3 branchement sur une case :
   + on explore le premier cas a droite
   + on revient car mauvaise solution et on passe au middle
   + egalement mauvaise solution /!\ a ne pas revenir a droite !

    | 0 -> 3                        0
    | 1 -> 0                      3   1
    | 2 -> 1                        2
    | 3 -> 2

+-> si mur on fait demi-tour et on prend a droite.
|
|
+-- si on vien d'un autre chemin on tourne direct a droite -> | 0 -> 1
                                                              | 1 -> 2
                                                              | 2 -> 3
                                                              | 3 -> 0
                                x      y      c
val set_color_at_pos : maze -> int * int -> int -> maze

**)

let run maze entry out =
  let comp_tuple (f1, s1) (f2, s2) =
    ((f1 = f2) && (s1 = s2))
  in

  let test_end current =
    function
      | true	-> (-42, -42)
      | _	-> current
  in

  let stat cur di = Case.statement (Maze.get_case_at_pos maze cur) di
  in

  let color_path old current dir =
    function
      | (0, _)	-> (test_end (Maze.set_color_at_pos maze current 1) (comp_tuple current out), dir, stat current dir)
      | (1, 1)  -> (Maze.set_color_at_pos maze old 0, dir, stat current dir)
      | _ -> (current, dir, Case.Wall)
   in

  let get_color pos = Case.color (Maze.get_case_at_pos maze pos)
  in

  let move_path =
    function
      | ((cx, cy), 0)	->
	begin
	  print_endline ("val : x = " ^ (string_of_int cx) ^ "; y = " ^ (string_of_int cy) ^ "\n");
	  color_path (cx, cy) (cx, cy - 1) 0 (get_color (cx, cy - 1), get_color (cx, cy))
	end
      | ((cx, cy), 1)	->
	begin
	  print_endline ("val : x = " ^ (string_of_int cx) ^ "; y = " ^ (string_of_int cy) ^ "\n");
	  color_path (cx, cy) (cx + 1, cy) 1 (get_color (cx - 1, cy), get_color (cx, cy))
	end
      | ((cx, cy), 2)	->
	begin
	  print_endline ("val : x = " ^ (string_of_int cx) ^ "; y = " ^ (string_of_int cy) ^ "\n");
	  color_path (cx, cy) (cx, cy + 1) 2 (get_color (cx, cy + 1), get_color (cx, cy))
	end
      | ((cx, cy), 3)	->
	begin
	  print_endline ("val : x = " ^ (string_of_int cx) ^ "; y = " ^ (string_of_int cy) ^ "\n");
	  color_path (cx, cy) (cx - 1, cy) 3 (get_color (cx - 1, cy), get_color (cx, cy))
	end
      | _		-> failwith "Execution fatal error"
  in

  let rec in_find =

    function
      | ((-42, -42), _, _)		-> maze
      | (current, dir, Case.Door)	->
	begin
	  Draw.print_maze_numbers maze 10 10;
	  in_find (move_path (current, dir))
	end
      | (current, 0, _)			-> in_find (current, 1, stat current 1)
      | (current, 1, _)			-> in_find (current, 2, stat current 2)
      | (current, 2, _)			-> in_find (current, 3, stat current 3)
      | (current, 3, _)			-> in_find (current, 0, stat current 0)
      | _				-> failwith "Execution fatal error"
  in

  in_find (entry, 0, (Case.statement (Maze.get_case_at_pos maze (Maze.set_color_at_pos maze entry 1)) 0))
;;
