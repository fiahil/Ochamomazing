(*
 *
 * Case.mli for A-Maze-ing
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

val numberSides : int
val color : case -> int
val statement : case -> int -> elt
val create : int -> case
val set_case_side : case -> elt -> int -> case
