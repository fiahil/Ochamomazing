(*
 *
 * Maze.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

type maze = Case.case array array

let create width high =
  let rec create_line line value =
    function
      | -1      -> line
      | n       ->
        begin
          Array.set line n (Case.create value);
          create_line line (value + 1) (n - 1)
        end
  in

  let rec create_map map width =
    function
      | -1      -> map
      | n       ->
        begin
          Array.set map n
            (create_line
               (Array.make width (Case.create (n * width)))
               (n * width)
               (width - 1));
          create_map map width (n - 1)
        end
  in

  create_map (Array.make high [||]) width (high - 1)

let get_case_at_pos maze (x, y) =
  Array.get (Array.get maze x) y

let set_color_at_pos maze (x, y) col =
  Array.set maze.(x) y (Case.set_color (get_case_at_pos maze (x, y)) col);
  (x, y)

let colorize maze width high =
  let get_first (a, _) = a
  in

  let get_second (_, b) = b
  in

  let inverse_tuple (x, y) = (-x, -y)
  in

  let get_rand_case () = (Random.int high, Random.int width)
  in

  let get_rand_dir () =
    match (Random.int 4) with
      | 0       -> (0, 1)
      | 1       -> (1, 0)
      | 2       -> (0, -1)
      | _       -> (-1, 0)
  in

  let check_position (x, y) =
    x > -1 && y > -1 && x < high && y < width
  in

  let add_tuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  in

  let change_case_by_color color new_color =
    let rec change_columns x =
      function
        | -1    -> ()
        | y     ->
          if (Case.color (get_case_at_pos maze (x, y)) = color)
          then
            begin
              Array.set maze.(x) y
                (Case.set_color (get_case_at_pos maze (x, y)) new_color);
              change_columns x (y - 1)
            end
          else
            change_columns x (y - 1)
    in

    let rec change_lines =
      function
        | -1    -> ()
        | x     ->
          begin
            change_columns x (width - 1);
            change_lines (x - 1)
          end
    in

    change_lines (high - 1)
  in

  let change_case case dir =
    let change_wall case =
      function
        | (1, 0)        -> Case.set_side case Case.Door 0
        | (0, -1)       -> Case.set_side case Case.Door 1
        | (-1, 0)       -> Case.set_side case Case.Door 2
        | (0, 1)        -> Case.set_side case Case.Door 3
        | _             -> failwith "Invalid direction tuple."
    and
        min x y =
      if x > y then
        y
      else
        x
    and
        max x y =
      if x > y then
        x
      else
        y
    in


    if (check_position (add_tuple dir case) &&
          (Case.color (get_case_at_pos maze case) !=
              Case.color (get_case_at_pos maze (add_tuple dir case))))
    then
      begin
        Array.set maze.(get_first (add_tuple dir case))
          (get_second (add_tuple dir case))
          (change_wall
             (get_case_at_pos maze (add_tuple dir case))
             (inverse_tuple dir));
        Array.set maze.(get_first case)
          (get_second case)
          (change_wall
             (get_case_at_pos maze case) dir);
        change_case_by_color
          (max (Case.color (get_case_at_pos maze (add_tuple dir case)))
             (Case.color (get_case_at_pos maze case)))
          (min (Case.color (get_case_at_pos maze (add_tuple dir case)))
             (Case.color (get_case_at_pos maze case)));
        1
      end
    else
      0
  in

  let rec change_cases =
    function
      | 0       -> ()
      | n       -> change_cases (n - (change_case (get_rand_case()) (get_rand_dir())))
  in

  change_cases (width * high - 1);
  maze

