(*
 * A-Maze-Ing project
 *
 * Benjamin Tourou
 *)

module type MAKEDRAW =
sig
  type t

  val print_maze : t -> (int * int) -> int -> int -> unit
end

module MakeDraw (Val : Maze.MAKEMAZE) : MAKEDRAW
  with
    type t = Val.maze =

struct
  type t = Val.maze

  let map_high          = ref 0
  let map_width         = ref 0
  let width_begin       = ref 0
  let high_begin        = ref 0
  let screen_width      = ref 1200
  let screen_high       = ref 800

  let draw_maze screen maze width high =

    let draw_img (x, y) sprite =   (* faire un draw something *)
      let position_of_image = Sdlvideo.rect
        (Val.Elt.calc_width_pos (x, y) !screen_width !width_begin)
        (Val.Elt.calc_high_pos x !screen_high !high_begin)
        0 0
      in
      Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:sprite ~dst:screen ()
    in

    let rec manage_draw_walls (x, y) =
      function
        | -1    -> ()
        | n     ->
          begin
            if (Val.Elt.statement (Val.get_case_at_pos maze (x, y)) n == Val.Elt.Wall)
            then
              draw_img (x, y) (Val.Elt.get_sprite n)
            else
              ();
            manage_draw_walls (x, y) (n - 1)
          end
    in

    let draw_case (x, y) =
      let position_of_path = Sdlvideo.rect
        (Val.Elt.calc_width_pos (x, y) !screen_width !width_begin)
        (Val.Elt.calc_high_pos x !screen_high !high_begin)
        0 0
      in

      draw_img (x, y) Val.Elt.empty;
      manage_draw_walls (x, y) (Val.Elt.numberSides - 1);
      if Val.get_color_at_pos maze (x, y) = 2 then
        Sdlvideo.blit_surface ~dst_rect:position_of_path ~src:Val.Elt.enter ~dst:screen ()
      else if Val.get_color_at_pos maze (x, y) = 3 then
        Sdlvideo.blit_surface ~dst_rect:position_of_path ~src:Val.Elt.out ~dst:screen ()
      else if Val.get_color_at_pos maze (x, y) != 0 then
        Sdlvideo.blit_surface ~dst_rect:position_of_path ~src:Val.Elt.path ~dst:screen ()
      else
        ()
    in

    let rec draw =
      function
        | (0, -1)       -> ()
        | (x, -1)       -> draw (x - 1, width - 1)
        | (x, y)        ->
          begin
            draw_case (x, y);
            draw (x, y - 1)
          end
    in

    let clear_screen =
      let clear = Sdlvideo.rect 0 0 !screen_width !screen_high in
      Sdlvideo.fill_rect ~rect: clear screen (Sdlvideo.map_RGB screen Sdlvideo.black);
    in
    clear_screen;
    draw (high - 1, width - 1);
    Sdlvideo.flip screen

  let wait_for_escape screen maze width high =
    let manage_scroll cur max value screen_size =
      if max - (cur + value) >= screen_size && (cur + value) >= 0 then
        cur + value
      else
        cur
    in

    let rec wait () =
      match Sdlevent.poll () with
	| None							-> wait ()
	| Some (Sdlevent.KEYDOWN ev | Sdlevent.KEYUP ev)	->
	  (match ev with
            | {Sdlevent.keysym = Sdlkey.KEY_ESCAPE}	-> ()
            | {Sdlevent.keysym = Sdlkey.KEY_UP}		->
              begin
		high_begin := manage_scroll !high_begin !map_high 25 !screen_high;
		draw_maze screen maze width high;
		wait ()
              end
            | {Sdlevent.keysym = Sdlkey.KEY_DOWN}		->
              begin
		high_begin := manage_scroll !high_begin !map_high (-25) !screen_high;
		draw_maze screen maze width high;
		wait ()
              end
            | {Sdlevent.keysym = Sdlkey.KEY_LEFT}		->
              begin
		width_begin := manage_scroll !width_begin !map_width 25 !screen_width;
		draw_maze screen maze width high;
		wait ()
              end
            | {Sdlevent.keysym = Sdlkey.KEY_RIGHT}		->
              begin
		width_begin := manage_scroll !width_begin !map_width (-25) !screen_width;
		draw_maze screen maze width high;
		wait ()
              end
            | _								-> wait ())
	| _							-> wait ()
    in
    wait ()

  let init_sdl high width =
    Sdl.init [`VIDEO];
    Sdlkey.enable_key_repeat ();
    Sdlvideo.set_video_mode !screen_width !screen_high [`DOUBLEBUF]

  let init_sizes =
    function
      | (true, true)    ->
        begin
          screen_width := !map_width;
          screen_high  := !map_high;
          high_begin := 0;
          width_begin := 0
        end
      | (true, false)   ->
        begin
          screen_width := !map_width;
          width_begin := 0;
          if (!high_begin > !map_high - !screen_high) then
            high_begin := !map_high - !screen_high
	  else if (!high_begin < 0) then
            high_begin := 0
        end
      | (false, true)   ->
        begin
          screen_high := !map_high;
          high_begin := 0;
          if (!width_begin > !map_width - !screen_width) then
            width_begin := !map_width - !screen_width
	  else if (!width_begin < 0) then
            high_begin := 0
        end
      | _                       ->
        begin
          if (!high_begin > !map_high - !screen_high) then
            high_begin := !map_high - !screen_high
	  else if (!high_begin < 0) then
            high_begin := 0;
          if (!width_begin > !map_width - !screen_width) then
            width_begin := !map_width - !screen_width
	  else if (!width_begin < 0) then
            width_begin := 0
        end

  let print_maze maze (ex, ey) width high =
    begin
      map_width := Val.Elt.calc_map_width width;
      map_high := Val.Elt.calc_map_high high;
      high_begin := Val.Elt.calc_begin_high ex !screen_high;
      width_begin := Val.Elt.calc_begin_width ey !screen_width;
      init_sizes (!map_width < !screen_width, !map_high < !screen_high);
      let screen = init_sdl high width
      in

      draw_maze screen maze width high;
      (* Sdltimer.delay 1000; *)
      wait_for_escape screen maze width high
    end
end
