(*
 *
 * Maze.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

type maze = Case.case array array

let create width high = (* gestion d'erreur sur la taille *)
  let rec create_line line value = function
    | -1 -> line
    | n ->
      begin
        Array.set line n (Case.create value); (*changer pour un Case.create*)
        create_line line (value + 1) (n - 1)
      end
  in

  let rec create_map map width = function
    | -1 -> map
    | n ->
      begin
        Array.set map n (create_line (Array.make width (Case.create (n * width)))
                           (n * width) (width - 1));
        create_map map width (n - 1)
      end
  in
  create_map (Array.make high [||]) width (high - 1)
;;

let get_case_at_pos maze (x, y) =
  Array.get (Array.get maze x) y
;;


let colorize maze width high =
  let get_first (a, _) = a in

  let get_second (_, b) = b in

  let inverse_tuple (x, y) = (-x, -y) in

  let get_rand_case rand =
    (rand * (Random.int high) mod high, rand * (Random.int width) mod width)  (* trouver comment randomiser en un appel *)
  in

  let get_rand_dir rand =
    match (rand * (Random.int 4) mod 4) with
      | 0 -> (0, 1)
      | 1 -> (1, 0)
      | 2 -> (0, -1)
      | _ -> (-1, 0)
  in

  let check_position (x, y) (dirx, diry) =
    x > dirx && y > diry && x < width + dirx && y < high + diry
  in

  let add_tuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) in

  let change_case case dir =

    let change_wall case = function
      | (0, -1) -> Case.set_side case Case.Door 0
      | (0, 1)  -> Case.set_side case Case.Door 1
      | (1, 0)  -> Case.set_side case Case.Door 2
      | (-1, 0) -> Case.set_side case Case.Door 3
      | _       -> failwith "Invalide direction tuple"
    in
    Printf.printf "%d %d\n" (get_first (add_tuple dir case)) (get_second (add_tuple dir case));
    if (check_position (add_tuple dir case) (inverse_tuple dir) && check_position case dir && Case.color (get_case_at_pos maze case) != Case.color (get_case_at_pos maze (add_tuple dir case)))
    then
      begin
        Array.set maze.(get_first (add_tuple dir case)) (get_second (add_tuple dir case)) (change_wall (get_case_at_pos maze (add_tuple dir case)) (inverse_tuple dir));  (* vraiment tres moche *)
        Array.set maze.(get_first case) (get_second case) (change_wall (get_case_at_pos maze case) dir);
        1
      end
    else
      begin
        Printf.printf "Don't\n";
        0
      end
  in

  let rec change_cases = function
    | 0 -> ()
    | n ->
      begin
        Printf.printf "Un tour %d\n" n;
        change_cases (n - change_case (get_rand_case (Random.int 100)) (get_rand_dir (Random.int 100)))
      end
  in

  change_cases (width * high);
  maze

let maze = create 10 10;;

let _ = colorize maze 10 10;;

let print_tmp maze width high =
  let print_line x =
    let rec print_l x = function
      | 0 -> ()
      | y ->
        begin
          let case = get_case_at_pos maze (x, y) in
          if Case.statement case 1 = Case.Wall then Printf.printf "_" else Printf.printf " ";
          if Case.statement case 2 = Case.Wall then Printf.printf "|" else Printf.printf " ";
          print_l x (y - 1)
        end
    in
    let case = get_case_at_pos maze (x, 0) in
    if Case.statement case 3 = Case.Wall then Printf.printf "|" else Printf.printf " ";
    print_l x (width - 1);
    Printf.printf "\n"
  in

  let rec print_c = function
    | 0 -> ()
    | n ->
      begin
        print_line n;
        print_c (n - 1)
      end
  in

  let rec print_first_line = function
    | 0 -> ()
    | y ->
      begin
        let case = get_case_at_pos maze (0, y) in
        Printf.printf " ";
        if Case.statement case 0 = Case.Wall then Printf.printf "_" else Printf.printf " ";
        print_first_line (y - 1)
      end
  in
  print_first_line (width - 1);
  Printf.printf "\n";
  print_c (high - 1)

let _ = print_tmp maze 10 10;;
