
open Graphics
open Graphics_utils
open Cmq

module Visual = 
    struct
        (** 描画用処理 *)
        let beam_x0 = 200
        let beam_x1 = 400
        let beam_y  = 300
        
        let draw_beam _ =
            Graphics.set_color black;
            Graphics.set_line_width 3;
            Graphics.moveto  beam_x0 beam_y;
            Graphics.lineto  beam_x1 beam_y
        
        let draw_arrow (x, y) = 
          Graphics.moveto  x  y;
          Graphics.lineto  x  beam_y;
          if y >= beam_y then (
              Graphics.rlineto (-10) (+10); 
              Graphics.moveto  x  beam_y;
              Graphics.rlineto (+10) (+10)
          )
          else (
              Graphics.rlineto (-10) (-10); 
              Graphics.moveto  x  beam_y;
              Graphics.rlineto (+10) (-10)
          )
        
        let draw_load (status) = 
            (* CMQの計算 *)
            let calc_cmq (x) = 
              let length = float_of_int (beam_x1 - beam_x0) in
              let pos    = float_of_int (      x - beam_x0) in
              let force  = 1.0 in
              Cmq.concentration_load ~l:length ~p:force ~x:pos
            in
        
            let (mouse_x, mouse_y) = (status.mouse_x, status.mouse_y) in
            let x = 
              if      mouse_x < beam_x0 then beam_x0
              else if mouse_x > beam_x1 then beam_x1
              else                           mouse_x
            in
            let y = beam_y + 50 in
            Graphics.set_color blue;
            Graphics.set_line_width 2;
            draw_arrow (x, y);
        
            let cmq = calc_cmq (mouse_x) in
            Graphics.moveto       50   500;
            Printf.sprintf "CL=%.2f" cmq.Cmq.ca |> Graphics.draw_string;
            Graphics.moveto       50   490;     
            Printf.sprintf "CR=%.2f" cmq.Cmq.cb |> Graphics.draw_string 

        (* 描画用処理 *)
        let visualize () = 
            let () = init "cmq" in
            default_actions 
            |> push (draw_beam)
            |> push (draw_load)
            |> run active_events

    end
;;

module App = 
    struct

        type config = {
            arg_index     : int;
            input_file    : string option;
            visualization : bool;
            sample_in     : bool;
            sample_out    : bool;
        }

        let init () = {
            arg_index     = 1;
            input_file    = None;
            visualization = false;
            sample_in     = false;
            sample_out    = false;
        }

        let sample_in () = 
            Printf.printf "## Definition of concentration load \n";
            Printf.printf "# x1:loading position\n";
            Printf.printf "# p1:loading force \n";
            Printf.printf "#  l:length of member \n";
            Printf.printf "Concentration p1:100.0 x1:2.0 l:5.0 \n";
            Printf.printf "## Definition of distribution load \n";
            Printf.printf "# x1:loading position of left side\n";
            Printf.printf "# x2:loading position of right side\n";
            Printf.printf "# p1:loading force at x1\n";
            Printf.printf "# p2:loading force at x2\n";
            Printf.printf "# l:length of member \n";
            Printf.printf "Distribution x1:2.0 x2:3.0 p1:50.0 p2:100.0 l:5.0\n";
            Printf.printf "## Summation of following load \n";
            Printf.printf "Sum\n";
            Printf.printf "## Quit computation \n";
            Printf.printf "End\n"

        let sample_out () = 
            Printf.printf "## Output of CMQ \n";
            Printf.printf "Cmq ca:150.0 cb:200.0 qa:100.0 qb:200.0 m0:50.0\n"

        let interprit config stack line =
            let open Parser in
            let (cmnd, attr_tokens) = Parser.split_cmnd_attrs (line) in
            let attrs = Parser.read_attr (attr_tokens) in
            match cmnd with
            | "Concentration" -> 
                let l  = attrs |> Attr.find "l" in
                let p1 = attrs |> Attr.find "p1" in
                let x1 = attrs |> Attr.find "x1" in
                let cmq =
                    ConcentrationLoad.create ~l ~p1 ~x1 
                    |> ConcentrationLoad.to_cmq
                in
                cmq::stack

            | "Distribution" -> 
                let l  = attrs |> Attr.find "l" in
                let p1 = attrs |> Attr.find "p1" in
                let p2 = attrs |> Attr.find "p2" in
                let x1 = attrs |> Attr.find "x1" in
                let x2 = attrs |> Attr.find "x2" in
                let cmq =
                    DistributionLoad.create ~l ~p1 ~p2 ~x1 ~x2 
                    |> DistributionLoad.to_cmq
                in
                cmq::stack

            | "Sum" -> 
                [ Cmq.sum (stack) ]

            | "End" -> 
                let cmq = Cmq.sum (stack) in
                let ()  = Cmq.print (cmq) in
                []

            | _ -> 
                stack

        let rec parse_args (args:string array) config : config =
            let length = Array.length (args) in 
            let index  = config.arg_index in
            if index < length then
                match args.(index) with
                | "-i" when index < length - 1 -> 
                    let file_name = args.(index + 1) in
                    { config with input_file = Some (file_name);
                                  arg_index  = index + 2 } 
                    |> parse_args (args) 

                | "--sample_in"  -> 
                    { config with sample_in = true; 
                                  arg_index = index + 1 }     
                    |> parse_args (args)

                | "--sample_out" ->  
                    { config with sample_out = true;     
                                  arg_index  = index + 1 }     
                    |> parse_args (args)

                | "--visual" -> 
                    { config with visualization = true; 
                                  arg_index     = index + 1 }     
                    |> parse_args (args)

                | _ -> 
                    { config with arg_index = index + 1 } 
                    |> parse_args (args) 
            else config

        let parse () = 
            let config    = init () |> parse_args (Sys.argv) in
            
            if config.sample_in  then sample_in ();
            if config.sample_out then sample_out ();
            
            match config.input_file with
            | Some (file_name) -> 
                (* ファイル入力 *)
                let in_channel = open_in (file_name) in
                let buf = ref [] in
                let rec read_to_end () =
                    let line = input_line in_channel in
                    buf := line::(!buf);
                    read_to_end ()
                in
                let lines = !buf |> List.rev in
                let () =
                    try read_to_end () with
                    | End_of_file -> close_in (in_channel)
                in

                (* 入力によって表示を切り替える *)
                if config.visualization then Visual.visualize ();
                
                let _ =
                    lines
                    |> List.fold_left (fun stack line -> 
                        line |> interprit config stack
                    ) [ (Cmq.empty ()) ]
                in
                ()

            | None ->
                (* 標準入力 *)
                let rec loop (stack) = 
                    match stack with
                    | [] -> ()
                    | _  ->
                        read_line ()  
                        |> interprit config stack 
                        |> loop
                in
                loop [ (Cmq.empty ()) ]


    end

;;

(* 
 Visual.visualize ()
*)

App.parse ()
