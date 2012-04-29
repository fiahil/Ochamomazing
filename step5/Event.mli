(*
 * A-Maze-Ing
 *
 * Fiahil
 *)

val set_mouse_func : (Sdlevent.mousebutton_event -> unit) -> unit
val set_key_func : (Sdlevent.keyboard_event -> bool) -> unit
val set_idle_func : (unit -> unit) -> unit

val loop : unit -> unit
