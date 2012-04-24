(*
 *
 * Display.ml for A-Maze-ing
 *
 * started by benjamin businaro - busina_b
 *
 *)

Sdl.init [`EVERYTHING];;

let (bpp, w, h) = (16, 320, 200);;
let screen = Sdlvideo.set_video_mode ~w ~h ~bpp [`HWSURFACE];; (* define some colors *)

let test =
begin
Sdlvideo.fill_rect screen (map_RGB screen white);
Sdlvideo.put_pixel screen ~x:160 ~y:100 red;
Sdlvideo.flip screen;
Sdl.quit()
end
;;

test;;
