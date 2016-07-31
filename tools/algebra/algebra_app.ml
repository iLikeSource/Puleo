
open Graphics
open Graphics_utils
open Algebra

module Visual = 
    struct
        let sample status = 
            let magnify = 100.0 in
            let (mouse_x, mouse_y) = (status.mouse_x, status.mouse_y) in
            let (draw_ox, draw_oy) = (w / 2, h / 2) in
            let v1 = ( 1.0,  2.0) in
            let v2 = ( float_of_int (mouse_x - draw_ox), float_of_int (mouse_y - draw_oy)) in
            let v3 = Vector.bisector v1 v2 in
            
            let () = 
                let (x, y) = v3 in
                Graphics.moveto (w - 100) (h - 100);
                Printf.sprintf "(%.3f, %.3f)" x y |> Graphics.draw_string 
            in
            let pos (x, y) =
                (draw_ox + int_of_float (x *. magnify),
                 draw_oy + int_of_float (y *. magnify))
            in
            let (draw_x0, draw_y0) = pos (0.0, 0.0) in
            let (draw_x1, draw_y1) = pos (v1) in
            let (draw_x2, draw_y2) = pos (v2) in
            let (draw_x3, draw_y3) = pos (v3) in
            Graphics.set_color black;
            Graphics.set_line_width 1;
            Graphics.moveto draw_x0 draw_y0;
            Graphics.lineto draw_x1 draw_y1;
            Graphics.moveto draw_x0 draw_y0;
            Graphics.lineto draw_x2 draw_y2;
            
            Graphics.set_color blue;
            Graphics.set_line_width 3;
            Graphics.moveto draw_x0 draw_y0;
            Graphics.lineto draw_x3 draw_y3;
            
            ()

        let visualize () = 
            let () = init "algebra" in
            default_actions
            |> push (sample)
            |> run active_events

    end

;;

Visual.visualize ()
