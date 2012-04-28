(*
 *
 * Case.mli for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

type elt =
  | Wall
  | Door

type case

val numberSides : int
val color : case -> int
val set_color : case -> int -> case
val get_sides : case -> (elt * elt * elt * elt)
val set_side : case -> elt -> int -> case
val statement : case -> int -> elt
val create : int -> case
