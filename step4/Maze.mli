(*
 *
 * Maze.mli for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

type maze

val create : int -> int -> maze
val colorize : maze -> int -> int -> maze
val get_case_at_pos : maze -> int * int -> Case.case
