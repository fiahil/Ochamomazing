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
  val set_color_at_pos : maze -> int * int -> int -> unit
  val get_color_at_pos : maze -> int * int -> int
  val clear_maze : maze -> unit
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
    Array.set maze.(x) y (Val.set_color (get_case_at_pos maze (x, y)) col)

  let get_color_at_pos maze (x, y) =
    Val.color (Array.get maze.(x) y)

  let colorize maze width high =
    let get_rand_case () = (Random.int high, Random.int width)
    in

    let get_rand_dir () = Val.get_dir_pattern (Random.int Elt.numberSides)
    in

    let check_position (x, y) =
      x > -1 && y > -1 && x < high && y < width
    in

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

    let rec change_cases =
      function
        | 0     -> ()
        | n     -> change_cases (n - (change_case (get_rand_case()) (get_rand_dir())))
    in

    change_cases (width * high - 1);
    maze

  let clear_maze maze =
    let rec clear_line =
      function
        | (_, -1)       -> ()
        | (x, y)        ->
          begin
            if Val.color (get_case_at_pos maze (x, y)) != 4 then
              set_color_at_pos maze (x, y) 0;
            clear_line (x, y - 1)
          end
    in

    let rec clear =
      function
        | -1    -> ()
        | x     ->
          begin
            clear_line (x, (Array.length maze.(0)) - 1);
            clear (x - 1)
          end
    in
    clear (Array.length maze - 1)

end
