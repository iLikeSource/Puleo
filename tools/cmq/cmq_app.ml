
open Graphics
open Graphics_utils
open Cmq

(*
 * TODO
 * - config で --process指定
 * - visual で荷重を可視化
 * - 可視化する行を上下キーで切り替え
 *)


module Interpriter =
    struct
        
        let to_load (line) = 
            let open Parser in
            let (cmnd, attr_tokens) = Parser.split_cmnd_attrs (line) in
            let attrs = Parser.read_attr (attr_tokens) in
            match cmnd with
            | "Concentration" -> 
                let l    = attrs |> Attr.find "l" in
                let p1   = attrs |> Attr.find "p1" in
                let x1   = attrs |> Attr.find "x1" in
                let load = ConcentrationLoad.create ~l ~p1 ~x1 in
                [ BeamLoad.Concentration (load) ]

            | "Distribution" -> 
                let l    = attrs |> Attr.find "l" in
                let p1   = attrs |> Attr.find "p1" in
                let p2   = attrs |> Attr.find "p2" in
                let x1   = attrs |> Attr.find "x1" in
                let x2   = attrs |> Attr.find "x2" in
                let load = DistributionLoad.create ~l ~p1 ~p2 ~x1 ~x2 in 
                [ BeamLoad.Distribution (load) ]

            | _ -> 
                []

        let interprit lines index =
            let line = lines.(index) in
            let (cmnd, attr_tokens) = Parser.split_cmnd_attrs (line) in
            let loads =
                match cmnd with
                | "Concentration" | "Distribution" -> 
                    to_load (line)
                | "Sum" | "End" -> 
                    if index = 0 
                    then []
                    else
                        Array.sub lines 0 index
                        |> Array.map (fun line -> to_load (line))    
                        |> Array.to_list
                        |> List.concat
                | _ -> []
            in
            let cmq = 
                loads 
                |> List.map (fun load -> BeamLoad.to_cmq (load)) 
                |> Cmq.sum 
            in
            let () = 
                if cmnd = "End" then Cmq.print (cmq)
            in    
            cmq
    end

module Visual = 
    struct
        (** 描画用処理 *)
        let beam_x0 = 200
        let beam_x1 = 400
        let beam_y  = 300

        (** 状態 *)
        let line_index_ref = ref 0
        
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
        
        (*
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
            Graphics.moveto       50   400;
            Printf.sprintf "CL=%.2f" cmq.Cmq.ca |> Graphics.draw_string;
            Graphics.moveto       50   390;     
            Printf.sprintf "CR=%.2f" cmq.Cmq.cb |> Graphics.draw_string 
        *)


        let draw_commands lines status = 
            let (x0, y0) = (20, h - 80) in
            lines
            |> Array.mapi (fun i line -> (i, line))
            |> Array.fold_left (fun (x, y) (i, line) ->
                Graphics.moveto x y;
                
                (* 色を設定 *)
                let () = 
                    if i = !line_index_ref 
                    then Graphics.set_color red
                    else Graphics.set_color black
                in

                Graphics.draw_string line;
                (x, y - 10)
            ) (x0, y0)
            |> ignore

        let draw_cmq lines status = 
            let cmq  = Interpriter.interprit lines !line_index_ref in 
            Graphics.moveto 20 50;
            Graphics.set_color blue;
            Graphics.draw_string (Cmq.to_string cmq)


        (* 描画用処理 *)
        let visualize (lines) = 
            let () = init "cmq" in
            default_actions 
            |> push (draw_beam)
            |> push (draw_commands (lines))
            |> push (draw_cmq (lines))
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

        type option_command = {
            key         : string;
            description : string;
            action      : string array * int -> config
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

        let parse (argv) = 
            let config = init () |> parse_args (argv) in
            
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
                let () =
                    try read_to_end () with
                    | End_of_file -> close_in (in_channel)
                in

                (* ファイル入力行 *)
                let lines = !buf |> List.rev |> Array.of_list in

                (* 入力によって表示を切り替える *)
                if config.visualization then 
                    Visual.visualize (lines);
                
                (* コマンドを解釈し計算実行 *)
                let last_index = Array.length (lines) - 1 in
                let _ = Interpriter.interprit lines last_index in
                ()

            | None ->
                (* 標準入力 *)
                let rec loop (lines) = 
                    let length = Array.length (lines) in
                    let line   = read_line () in
                    let index  = length in
                    let new_lines  = 
                        Array.init (length + 1) begin fun i ->
                            if i = index 
                            then line
                            else lines.(i) 
                        end
                    in
                    let _ = Interpriter.interprit new_lines index in 
                    match new_lines.(index) with
                    | "End" -> ()
                    | _  ->
                        loop (new_lines)
                in
                loop [||]

    end

;;

(* 
 Visual.visualize ()
*)

let argv = Sys.argv in
if Array.length (argv) > 0 then
    App.parse (argv)
else
    Printf.printf "Usage:"

