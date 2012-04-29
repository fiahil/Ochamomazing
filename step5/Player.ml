(*
 * A-Maze-Ing
 *
 * Fiahil
 *)

module type MAKEPLAYER =
sig
  type t

  val init : t -> int * int -> unit
  val move : t -> unit
  val dir : unit -> int
  val pos : unit -> int * int
end

module MakePlayer (Val : Maze.MAKEMAZE) : MAKEPLAYER
  with
    type t = Val.maze =

struct
  type t = Val.maze

  let ppos = ref (0, 0)
  let player_dir = ref 0

  let init maze pos =
    begin
      ppos := pos;
      Val.set_color_at_pos maze pos 2
    end

  let move maze =
    begin
      player_dir := 0;
      let rec aux maze =
	if !player_dir >= Val.Elt.numberSides then
	  ()
	else if Val.Elt.statement (Val.get_case_at_pos maze !ppos) !player_dir !=
	       Val.Elt.Wall
	then
	  let case =
	    Val.Elt.get_adj_case !ppos (Val.Elt.get_dir_pattern !player_dir)
	  in

	  if Val.get_color_at_pos maze case = 1 ||
	    Val.get_color_at_pos maze case = 3
	  then
	    begin
	      Val.set_color_at_pos maze !ppos 0;
	      ppos := case;
	      Val.set_color_at_pos maze !ppos 2
	    end
	  else if Val.get_color_at_pos maze case = 5
	  then
	    begin
	      Val.set_color_at_pos maze !ppos 0;
	      ppos := case;
	      Val.set_color_at_pos maze !ppos 2;
	      print_endline ".:: Congratulation !! You Win !! ::.";
	      raise Exit
	    end
	  else if Val.get_color_at_pos maze case = 4
	  then
	    begin
	      Val.set_color_at_pos maze !ppos 0;
	      ppos := case;
	      Val.set_color_at_pos maze !ppos 2;
	      print_endline ".:: So Sad ... You Died ... ::.";
	      raise Exit
	    end
	  else
	    begin
	      player_dir := !player_dir + 1;
	      aux maze
	    end
	  else
	    begin
	      player_dir := !player_dir + 1;
	      aux maze
	    end
      in

      aux maze
    end

  let dir () =
    !player_dir

  let pos () =
    !ppos
end
