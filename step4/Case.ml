(*
 *
 * Case.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

module type CASE =
sig
  type case
  type elt =
    | Wall
    | Door

  val numberSides : int
  val empty : Sdlvideo.surface
  val get_sprite : int -> Sdlvideo.surface
  val color : case -> int
  val set_color : case -> int -> case
  val set_side : case -> elt -> int -> case
  val get_adj_case : int * int -> int * int -> int * int
  val get_opposed_wall : int * int -> int * int
  val get_dir_pattern : int -> int * int
  val set_dir_pattern : case -> int * int -> case
  val statement : case -> int -> elt
  val create : int -> case
  val calc_width_pos : int * int -> int -> int -> int
  val calc_high_pos : int -> int -> int -> int
end

module Square : CASE =
struct
  type elt =
    | Wall
    | Door

  type case = {color: int; sides: (elt * elt * elt * elt)}

  let numberSides = 4

  let empty  = Sdlloader.load_image "./img/Square/base.jpg"
  let wall_0  = Sdlloader.load_image "./img/Square/Wall_0.png"
  let wall_1  = Sdlloader.load_image "./img/Square/Wall_1.png"
  let wall_2  = Sdlloader.load_image "./img/Square/Wall_2.png"
  let wall_3  = Sdlloader.load_image "./img/Square/Wall_3.png"

  let color c = c.color

  let set_color {color = c; sides = si} col = {color = col; sides = si}

  let get_sides {color = c; sides = si} =
    si

  let statement {color = col; sides = (n, e, s, w)} =
    function
      | 0 -> n
      | 1 -> e
      | 2 -> s
      | 3 -> w
      | _ -> failwith "A Square case have only 4 sides."

  let set_side {color = col ; sides = (n, e, s, w)} elt_Type =
    function
      | 0 -> {color = col; sides = (elt_Type, e, s, w)}
      | 1 -> {color = col; sides = (n, elt_Type, s, w)}
      | 2 -> {color = col; sides = (n, e, elt_Type, w)}
      | 3 -> {color = col; sides = (n, e, s, elt_Type)}
      | _ -> failwith "A Square case have only 4 sides."

  let create col = {color = col; sides = (Wall, Wall, Wall, Wall)}

  let get_opposed_wall (x, y) =
    (-x, -y)

  let get_adj_case (x, y) (xdir, ydir) =
    (x + xdir, y + ydir)

  let get_dir_pattern =
    function
      | 0       -> (1, 0)
      | 1       -> (0, -1)
      | 2       -> (-1, 0)
      | _       -> (0, 1)

  let set_dir_pattern case =
    function
      | (1, 0)  -> set_side case Door 0
      | (0, -1) -> set_side case Door 1
      | (-1, 0) -> set_side case Door 2
      | (0, 1)  -> set_side case Door 3
      | _       -> failwith "Invalid direction pattern."

  let get_sprite =
    function
      | 0       -> wall_0
      | 1       -> wall_1
      | 2       -> wall_2
      | 3       -> wall_3
      | _       -> failwith "Invalid sprite number asked"

  let calc_width_pos (x, y) sc_size sc_begin =
    (sc_size - (50 * y) - 50 + sc_begin)

  let calc_high_pos x sc_size sc_begin =
    (sc_size - (50 * x) - 50 + sc_begin)

end

module Hexa : CASE =
struct
  type elt =
    | Wall
    | Door

  type case = {color: int; sides: (elt * elt * elt * elt * elt * elt)}

  let numberSides = 6

  let empty  = Sdlloader.load_image "./img/Hexa/base.png"
  let wall_0 = Sdlloader.load_image "./img/Hexa/Wall_0.png"
  let wall_1 = Sdlloader.load_image "./img/Hexa/Wall_1.png"
  let wall_2 = Sdlloader.load_image "./img/Hexa/Wall_2.png"
  let wall_3 = Sdlloader.load_image "./img/Hexa/Wall_3.png"
  let wall_4 = Sdlloader.load_image "./img/Hexa/Wall_4.png"
  let wall_5 = Sdlloader.load_image "./img/Hexa/Wall_5.png"

  let color c = c.color

  let set_color {color = c; sides = si} col = {color = col; sides = si}

  let statement {color = col; sides = (n, ne, se, s, sw, nw)} =
    function
      | 0 -> n
      | 1 -> ne
      | 2 -> se
      | 3 -> s
      | 4 -> sw
      | 5 -> nw
      | _ -> failwith "A Hexagone case has only 6 sides."

  let set_side {color = col ; sides = (n, ne, se, s, sw, nw)} elt_Type =
    function
      | 0       -> {color = col; sides = (elt_Type, ne, se, s, sw, nw)}
      | 1       -> {color = col; sides = (n, elt_Type, se, s, sw, nw)}
      | 2       -> {color = col; sides = (n, ne, elt_Type, s, sw, nw)}
      | 3       -> {color = col; sides = (n, ne, se, elt_Type, sw, nw)}
      | 4       -> {color = col; sides = (n, ne, se, s, elt_Type, nw)}
      | 5       -> {color = col; sides = (n, ne, se, s, sw, elt_Type)}
      | _       -> failwith "A Hexagone case has only 6 sides."

  let create col = {color = col; sides = (Wall, Wall, Wall, Wall, Wall, Wall)}

  let get_dir_pattern =
    function
      | 0     -> (0, 0)
      | 1     -> (0, -1)
      | 2     -> (0, 1)
      | 3     -> (1, 1)
      | 4     -> (1, 0)
      | _     -> (-1, 0)

  let get_opposed_wall =
    function
      | (0, 0)  -> (1, 1)
      | (0, -1) -> (1, 0)
      | (0, 1)  -> (-1, 0)
      | (1, 1)  -> (0, 0)
      | (1, 0)  -> (0, -1)
      | (-1, 0) -> (0, 1)
      | _       -> failwith "Invalid direction pattern."

  (* let get_adj_case (x, y) = *)
  (* function *)
  (* | (0, 0)  -> (y, x + 2) *)
  (* | (0, -1) -> (y - (x mod 2), x + 1) *)
  (* | (0, 1)  -> (y - (x mod 2), x - 1) *)
  (* | (1, 1)  -> (y, x - 2) *)
  (* | (1, 0)  -> (y + 1 - (x mod 2), x - 1) *)
  (* | (-1, 0) -> (y + 1 - (x mod 2)x + 1) *)
  (* | _       -> failwith "Invalid direction pattern." *)

  let get_adj_case (x, y) =
    function
      | (0, 0)  -> (x + 2, y)
      | (0, -1) -> (x + 1, y - (x mod 2))
      | (0, 1)  -> (x - 1, y - (x mod 2))
      | (1, 1)  -> (x - 2, y)
      | (1, 0)  -> (x - 1, y + 1 - (x mod 2))
      | (-1, 0) -> (x + 1, y + 1 - (x mod 2))
      | _       -> failwith "Invalid direction pattern."

  let set_dir_pattern case =
    function
      | (0, 0)       -> set_side case Door 0
      | (0, -1)      -> set_side case Door 1
      | (0, 1)       -> set_side case Door 2
      | (1, 1)       -> set_side case Door 3
      | (1, 0)       -> set_side case Door 4
      | (-1, 0)      -> set_side case Door 5
      | _     -> failwith "Invalid direction pattern."

  let get_sprite =
    function
      | 0       -> wall_0
      | 1       -> wall_1
      | 2       -> wall_2
      | 3       -> wall_3
      | 4       -> wall_4
      | 5       -> wall_5
      | _       -> failwith "Invalid wall asked."

  let calc_width_pos (x, y) sc_size sc_begin =
    (sc_size - (76 * y) - 100 + sc_begin + (x mod 2) * 38)

  let calc_high_pos x sc_size sc_begin =
    (sc_size - (22 * x) - 43 + sc_begin)

end
