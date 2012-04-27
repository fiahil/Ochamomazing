open Sdlevent
open Sdlkey

let screen_high  = 400
let screen_width = 400
let empty_0  = Sdlloader.load_image "./images/126.png"
let empty_1  = Sdlloader.load_image "./images/126.png"
let empty_2  = Sdlloader.load_image "./images/126.png"
let empty_3  = Sdlloader.load_image "./images/126.png"
let empty_4  = Sdlloader.load_image "./images/126.png"
let empty_5  = Sdlloader.load_image "./images/126.png"
let empty_6  = Sdlloader.load_image "./images/126.png"
let empty_7  = Sdlloader.load_image "./images/126.png"
let empty_8  = Sdlloader.load_image "./images/126.png"
let empty_9  = Sdlloader.load_image "./images/126.png"
let empty_10 = Sdlloader.load_image "./images/126.png"
let empty_11 = Sdlloader.load_image "./images/126.png"
let empty_12 = Sdlloader.load_image "./images/126.png"
let empty_13 = Sdlloader.load_image "./images/126.png"
let empty_14 = Sdlloader.load_image "./images/126.png"

let rec wait_for_escape () =
  match wait_event () with
    | KEYDOWN {keysym=KEY_ESCAPE} ->
      print_endline "You pressed escape! The fun is over now."
    | event ->
      print_endline (string_of_event event);
      wait_for_escape ()

let draw_maze screen maze width high =

  let pick_sprite =
    function
      | (Case.Door, Case.Door, Case.Door, Case.Door)        -> empty_0
      | (Case.Wall, Case.Door, Case.Door, Case.Door)        -> empty_1
      | (Case.Door, Case.Door, Case.Door, Case.Wall)        -> empty_2
      | (Case.Door, Case.Wall, Case.Door, Case.Door)        -> empty_3
      | (Case.Door, Case.Door, Case.Wall, Case.Door)        -> empty_4
      | (Case.Wall, Case.Door, Case.Door, Case.Wall)        -> empty_5
      | (Case.Wall, Case.Wall, Case.Door, Case.Door)        -> empty_6
      | (Case.Door, Case.Door, Case.Wall, Case.Wall)        -> empty_7
      | (Case.Door, Case.Wall, Case.Wall, Case.Door)        -> empty_8
      | (Case.Door, Case.Wall, Case.Door, Case.Wall)        -> empty_9
      | (Case.Wall, Case.Door, Case.Wall, Case.Door)        -> empty_10
      | (Case.Door, Case.Wall, Case.Wall, Case.Wall)        -> empty_11
      | (Case.Wall, Case.Wall, Case.Door, Case.Wall)        -> empty_12
      | (Case.Wall, Case.Wall, Case.Wall, Case.Door)        -> empty_13
      | (Case.Wall, Case.Door, Case.Wall, Case.Wall)        -> empty_14
      | _                                                   -> failwith "Invalid wall combination"
  in

  let draw_case (x, y) =
    let position_of_image = Sdlvideo.rect
      (screen_width / width * x)
      (screen_high / high * y)
      0 0 in
    Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:
      (pick_sprite (Case.get_sides (Maze.get_case_at_pos maze (x, y))))
      ~dst:screen ()
  in

  let rec draw =
    function
      | (0, -1)        -> ()
      | (x, -1)         -> draw (x - 1, width - 1)
      | (x, y)          ->
        begin
          draw_case (x, y);
          draw (x, y - 1)
        end
  in

  draw (high - 1, width - 1);
  Sdlvideo.flip screen;
  Sdltimer.delay 5000 (* play *)

let init_sdl =
  Sdl.init [`VIDEO];
  (* at_exit Sdl.quit; *)
  Sdlvideo.set_video_mode screen_width screen_high [`DOUBLEBUF]

let print_maze maze width high =
  let screen = init_sdl in
  draw_maze screen maze width high
