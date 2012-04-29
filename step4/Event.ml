(*
 * A-Maze-Ing
 *
 * Fiahil
 *)

let idle_func  = ref (fun () -> ())
let key_func   = ref (fun _ -> true)
let mouse_func = ref (fun _ -> ())

let rec loop () =
  let rec aux () =
    begin
      Sdltimer.delay 100;
      loop ()
    end
  and
      mouse ev =
    begin
      !mouse_func ev;
      aux ()
    end
  and
      key ev =
    begin
      if !key_func ev then
	aux ()
      else
	()
    end
  and
      idle () =
    begin
      !idle_func ();
      aux ()
    end

  in

  match Sdlevent.poll () with
    | None				 -> idle ()
    | Some (Sdlevent.KEYDOWN ev)	 -> key ev
    | Some (Sdlevent.MOUSEBUTTONDOWN ev) -> mouse ev
    | Some (Sdlevent.QUIT)		 -> ()
    | _					 -> aux ()

let set_mouse_func f =
  mouse_func := f

let set_key_func f =
  key_func := f

let set_idle_func f =
  idle_func := f
