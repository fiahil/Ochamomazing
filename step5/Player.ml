(*
 * A-Maze-Ing
 *
 * Fiahil
 *)

module type MAKEPLAYER =
sig
  type t

  val init : t -> int * int -> int * int
  val move : t -> unit
  val dir : unit -> int
  val pos : unit -> int * int
end

module MakePlayer (Val : Maze.MAKEMAZE) : MAKEPLAYER
  with
    type t = Val.maze =

struct
  type t = Val.maze

  let player_pos = ref (0, 0)
  let player_dir = ref 0

  let init maze pos =
    begin
      player_pos := pos;
      Val.set_color_at_pos maze pos 2
    end

  let move maze =
    let rec aux maze dir =
      if dir >= Val.Elt.numberSides then
	()
      else
	let case = Val.Elt.get_adj_case !player_pos (Val.Elt.get_dir_pattern dir)
	in

	if Val.get_color_at_pos maze case = 0 then
	  aux maze (dir + 1)
	else
	  begin
	    ignore (Val.set_color_at_pos maze !player_pos 0);
	    player_pos := case;
	    ignore (Val.set_color_at_pos maze !player_pos 2)
	  end
    in

    aux maze !player_dir

  let dir () =
    !player_dir

  let pos () =
    !player_pos
end
