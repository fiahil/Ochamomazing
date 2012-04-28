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

  let create width high =
    let rec create_line line value =
      function
        | -1    -> line
        | n     ->
          begin
            Array.set line n (Val.create value);
            create_line line (value + 1) (n - 1)
          end
    in

    let rec create_map map width =
      function
        | -1    -> map
        | n     ->
          begin
            Array.set map n
              (create_line
                 (Array.make width (Val.create (n * width)))
                 (n * width)
                 (width - 1));
            create_map map width (n - 1)
          end
    in

    create_map (Array.make high [||]) width (high - 1)

  let get_case_at_pos maze (x, y) =
    Array.get (Array.get maze x) y

  let set_color_at_pos maze (x, y) col =
    Array.set maze.(x) y (Val.set_color (get_case_at_pos maze (x, y)) col);
    (x, y)

  let colorize maze width high =
    (* let inverse_tuple (x, y) = (-x, -y) *)
    (* in *)

    let get_rand_case () = (Random.int high, Random.int width)
    in

    let get_rand_dir () = Val.get_dir_pattern (Random.int Elt.numberSides)
    in

    let check_position (x, y) =
      x > -1 && y > -1 && x < high && y < width
    in

    (* let add_tuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  (\* virer ? *\) *)
    (* in *)

    let change_case_by_color color new_color =
      let rec change_columns x =
        function
          | -1  -> ()
          | y   ->
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
          | -1  -> ()
          | x   ->
            begin
              change_columns x (width - 1);
              change_lines (x - 1)
            end
      in

      change_lines (high - 1)
    in

    let change_case case dir =
      let change_wall case (x, y) =
        Val.set_dir_pattern case (x, y)
      in

      if (check_position (Val.get_adj_case case dir) &&
            (Val.color (get_case_at_pos maze case) !=
                Val.color (get_case_at_pos maze (Val.get_adj_case case dir))))
      then
        begin
          Array.set maze.(fst (Val.get_adj_case case dir))
            (snd (Val.get_adj_case case dir))
            (change_wall
               (get_case_at_pos maze (Val.get_adj_case case dir))
               (Val.get_opposed_wall dir));
          Array.set maze.(fst case)
            (snd case)
            (change_wall
               (get_case_at_pos maze case) dir);
          change_case_by_color
            (max (Val.color (get_case_at_pos maze (Val.get_adj_case case dir)))
               (Val.color (get_case_at_pos maze case)))
            (min (Val.color (get_case_at_pos maze (Val.get_adj_case case dir)))
               (Val.color (get_case_at_pos maze case)));
          1
        end
      else
        0
    in

    let print_maze_numbers maze width high =
      let print_line x =
        let rec print_l x =
          function
            | -1        -> ()
            | y ->
              begin
                let case = get_case_at_pos maze (x, y) in
                Printf.printf "%2d " (Val.color case);
                print_l x (y - 1)
              end
        in

        print_l x (width - 1);
        Printf.printf "\n"
      in

      let rec print_c =
        function
          | -1  -> ()
          | n   ->
            begin
              print_line n;
              print_c (n - 1)
            end
      in

      let rec print_first_line =
        function
          | -1  -> ()
          | y   ->
            begin
              let case = get_case_at_pos maze (0, y)
              in

              Printf.printf "%d " (Val.color case);
              print_first_line (y - 1)
            end
      in

      print_c (high - 1);
      Printf.printf "\n"
    in


    let print_maze_state maze width high =
      let print_line x =
        let rec print_l x =
          function
            | -1        -> ()
            | y ->
              begin
                let case = get_case_at_pos maze (x, y)
                and
                    print_st =
                  function
                    | Val.Wall      -> Printf.printf "Wall "
                    | Val.Door      -> Printf.printf "Door "
                in

                Printf.printf "%2d " (Val.color case);
                print_st (Val.statement case 0);
                print_st (Val.statement case 1);
                print_st (Val.statement case 2);
                print_st (Val.statement case 3);
                print_st (Val.statement case 4);
                print_st (Val.statement case 5);
                Printf.printf "\n";
                print_l x (y - 1)
              end
        in

        print_l x (width - 1);
      in

      let rec print_c =
        function
          | -1  -> ()
          | n   ->
            begin
              print_line n;
              print_c (n - 1)
            end
      in

      let rec print_first_line =
        function
          | -1  -> ()
          | y   ->
            begin
              let case = get_case_at_pos maze (0, y)
              in

              Printf.printf "%d " (Val.color case);
              print_first_line (y - 1)
            end
      in

      print_c (high - 1);
      Printf.printf "\n"
    in

    let rec change_cases =
      function
        | 0     -> ()
        | n     ->
          begin
            print_maze_numbers maze width high;
            (* print_maze_state maze width high; *)
            change_cases (n - (change_case (get_rand_case()) (get_rand_dir())))
          end
    in

    change_cases (width * high - 1);
    maze
end
