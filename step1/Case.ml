(*
 *
 * Case.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

type elt =
  | Wall
  | Border
  | Door

type case =
    {
      color: int;
      sides: (elt * elt * elt * elt)
    }

let numberSides = 4

let color c =
  c.color

let statement {color = col ; sides = (n, s, e, w)} =
  function
    | 0 -> n
    | 1 -> s
    | 2 -> e
    | 3 -> w
    | _ -> failwith "A case have only 4 sides"

let set_side {color = col ; sides = (n, s, e, w)} elt_Type =
  function
    | 0 -> {color = col ; sides = (elt_Type, s, e, w)}
    | 1 -> {color = col ; sides = (n, elt_Type, e, w)}
    | 2 -> {color = col ; sides = (n, s, elt_Type, w)}
    | 3 -> {color = col ; sides = (n, s, e, elt_Type)}
    | _ -> failwith "A case have only 4 sides"

let create col =
  {color = col; sides = (Wall, Wall, Wall, Wall)}
