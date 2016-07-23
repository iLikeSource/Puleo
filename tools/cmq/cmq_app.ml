
open Graphics
open Graphics_utils
open Cmq

(*
 * TODO
 * - config で --process指定
 * - visual で荷重を可視化
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

        let to_loads lines index = 
            let line = lines.(index) in
            let (cmnd, attr_tokens) = Parser.split_cmnd_attrs (line) in
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

        let interprit ?(print=true) lines index =
            let line = lines.(index) in
            let (cmnd, attr_tokens) = Parser.split_cmnd_attrs (line) in
            let loads = to_loads lines index in
            let cmq = 
                loads 
                |> List.map (fun load -> BeamLoad.to_cmq (load)) 
                |> Cmq.sum 
            in
            let () = 
                if cmnd = "End" && print then Cmq.print (cmq)
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
        
        (** 集中荷重を描画 *)
        let draw_concentration_load (c_load:ConcentrationLoad.t) = 
            Graphics.set_line_width 3;
            let open ConcentrationLoad in
            let x = 
                let x1 = c_load.x1 in
                let l  = c_load.l in
                let beam_length = float_of_int (beam_x1 - beam_x0) in
                beam_x0 + int_of_float (beam_length *. (x1 /. l))
            in
            let p = c_load.p1 in
            let load_start_y = beam_y + 40 in
            Graphics.moveto  x  load_start_y;
            Graphics.lineto  x  beam_y;
            let () = 
                Graphics.rlineto (-10) (+10); 
                Graphics.moveto  x  beam_y;
                Graphics.rlineto (+10) (+10)
            in
            Graphics.moveto x (load_start_y + 10);
            Printf.sprintf "%.2f" p |>  draw_string 

        let draw_distribution_load (d_load:DistributionLoad.t) = 
            Graphics.set_line_width 1;
            let open DistributionLoad in
            let (x1, x2) = 
                let x1 = d_load.x1 in
                let x2 = d_load.x2 in
                let l  = d_load.l in
                let beam_length = float_of_int (beam_x1 - beam_x0) in
                (beam_x0 + int_of_float (beam_length *. (x1 /. l)),
                 beam_x0 + int_of_float (beam_length *. (x2 /. l)))
            in
            let (p1, p2) = (d_load.p1, d_load.p2) in
            let line_grad = 10 * int_of_float (p2 -. p1) / (x2 - x1) in
            let beam_y1 = beam_y + 20 in
            let f (x) = beam_y1 + (line_grad * (x - x1)) in
            let load_start_y1 = f (x1) in 
            let load_start_y2 = f (x2) in
            Graphics.moveto  x1  load_start_y1;
            Graphics.lineto  x2  load_start_y2;
            
            let rec draw_arrow x f = 
                let draw x =
                    let start_y = f (x) in
                    Graphics.moveto  x  start_y;
                    Graphics.lineto  x  beam_y;
                    Graphics.rlineto (-5) (+5); 
                    Graphics.moveto  x  beam_y;
                    Graphics.rlineto (+5) (+5)
                in
                if x2 < x 
                then draw (x2)
                else ( 
                    draw (x);
                    draw_arrow (x + 10) f
                )
            in
            draw_arrow x1 f


        (** 荷重を表示 *)
        let draw_load lines status = 
            Graphics.set_color red;
            Interpriter.to_loads lines !line_index_ref
            |> List.iter (fun load ->
                match load with
                | BeamLoad.Concentration c_load -> draw_concentration_load (c_load)
                | BeamLoad.Distribution  d_load -> draw_distribution_load  (d_load)
            )


        (** 入力されているコマンドを表示 *)
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
            let cmq  = Interpriter.interprit ~print:false lines !line_index_ref in 
            Graphics.moveto 20 50;
            Graphics.set_color blue;
            Graphics.draw_string (Cmq.to_string cmq)
        
        let key_action lines status = 
            let length = Array.length (lines) in 
            match status.key with
            | 'u' -> line_index_ref := (!line_index_ref + length - 1) mod length
            | 'd' -> line_index_ref := (!line_index_ref + length + 1) mod length
            | _   -> ()

        (* 描画用処理 *)
        let visualize (lines) = 
            let () = init "cmq" in
            default_actions 
            |> push (key_action (lines))
            |> push (draw_beam)
            |> push (draw_commands (lines))
            |> push (draw_load (lines))
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

