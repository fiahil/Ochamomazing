(*
 *
 * Case.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

type elt =
  | Wall
  | Door

type case = {color: int; sides: (elt * elt * elt * elt)}

let numberSides = 4

let color c =
  c.color

let set_color {color = c; sides = si} col =
  {color = col; sides = si}

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

let create col =
  {color = col; sides = (Wall, Wall, Wall, Wall)}
