
open Graphics;;

(** Fields *)
let active_events = [ Mouse_motion; Key_pressed ];;
let (w, h)        = (600, 600);;

let draw_pointer (status) = 
    draw_circle status.mouse_x status.mouse_y 10;
    moveto status.mouse_x status.mouse_y;
    let (x, y) = (status.mouse_x, status.mouse_y) in
    draw_string (Printf.sprintf "%d,%d" x y) 
;;

let default_actions : (Graphics.status -> unit) list = 
    [ 
        (fun (status) -> set_color black);
        (fun (status) -> set_line_width 1)
    ]
    |> List.rev
;;

let push f actions = 
    f :: actions 
;;

(** Function *)
let window_size (w, h) = 
    Printf.sprintf " %dx%d" w h
;;

let init (title) =
    open_graph (window_size (w, h));
    set_window_title title;
;;

let run active_events actions = 
    let rec run_rec () =
        let status = wait_next_event active_events in
        if status.keypressed && status.key = 'q' then ()
        else begin
            clear_graph ();
            actions |> List.rev |> List.iter (fun f -> f (status));
            run_rec ()
        end
    in
    run_rec ()
;;

(** Main *)
(*
init ("sample");
default_actions |> push [] |> run active_events;
*)

