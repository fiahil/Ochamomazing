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
  val color : case -> int
  val set_color : case -> int -> case
  val set_side : case -> elt -> int -> case
  val get_dir_pattern : int -> int * int
  val set_dir_pattern : case -> int * int -> case
  val statement : case -> int -> elt
  val create : int -> case
end

module Case =
struct
  type elt =
    | Wall
    | Door

  type case = {color: int; sides: (elt * elt * elt * elt)}

  let numberSides = 4

  let color c = c.color

  let set_color {color = c; sides = si} col = {color = col; sides = si}

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

  let get_dir_pattern =
    function
      | 0	-> (0, 1)
      | 1	-> (1, 0)
      | 2	-> (0, -1)
      | _	-> (-1, 0)

  let set_dir_pattern case =
    function
      | (1, 0)	-> set_side case Door 0
      | (0, -1)	-> set_side case Door 1
      | (-1, 0)	-> set_side case Door 2
      | (0, 1)	-> set_side case Door 3
      | _	-> failwith "Invalid direction pattern."
end
