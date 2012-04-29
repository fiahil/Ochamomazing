(*
 * A-Maze-Ing
 *
 * Fiahil
 *)

val mouse_func : (Sdlevent.mousebutton_event -> unit) ref
val key_func : (Sdlevent.keyboard_event -> unit) ref
val idle_func : (unit -> unit) ref

val set_mouse_func : (Sdlevent.mousebutton_event -> unit) -> unit
val set_key_func : (Sdlevent.keyboard_event -> unit) -> unit
val set_idle_func : (unit -> unit) -> unit

val loop : unit -> unit
