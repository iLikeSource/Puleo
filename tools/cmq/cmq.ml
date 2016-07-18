

(** CMQ計算 *)
module Cmq = 
    struct
        (** 型定義 *)
        type t = {
            ca : float;
            cb : float;
            qa : float;
            qb : float;
            m0 : float
        }

        (** 空のCMQ *)
        let empty () = {
            ca = 0.0;
            cb = 0.0;
            qa = 0.0;
            qb = 0.0;
            m0 = 0.0 
        }

        let print t = 
            Printf.printf "Cmq ca:%.2f cb:%.2f qa:%.2f qb:%.2f m0:%.2f"
                          t.ca t.cb t.qa t.qb t.m0

        (** CMQ荷重を加算する *)
        let add (x, y) = {
            ca = x.ca +. y.ca;
            cb = x.cb +. y.cb;
            qa = x.qa +. y.qa;
            qb = x.qb +. y.qb;
            m0 = x.m0 +. y.m0 
        }

        (** CMQ荷重を合計する *)
        let sum xs = 
            xs |> List.fold_left (fun dst src -> add (dst, src)) (empty ())

        (** 集中荷重 *)
        let concentration_load ~l ~p ~x = 
            let (a, b) = (x, l -. x) in
            let ca = -. p *. a        *. b ** 2.0 /. (l ** 2.0) in
            let cb = -. p *. a ** 2.0 *. b        /. (l ** 2.0) in
            let qa =    p *. b ** 2.0 *. (3.0 *. a +.        b) /. (l ** 3.0) in
            let qb =    p *. a ** 2.0 *. (       a +. 3.0 *. b) /. (l ** 3.0) in
            let m0 =    p *. a *. b /. l in
            { ca; cb; qa; qb; m0 }

        (** 積分 *)
        let integration ?(div=1000) ~l ~f = fun (x1, x2) ->
            let range = x2 -. x1 in
            let dx    = range /. (float_of_int div) in
            
            (* dx区間に作用する荷重 *)
            let p (x) = f (x) /. dx in
            
            (* 集中荷重として計算 *)
            Array.init div (fun i -> dx *. (0.5 +. float_of_int (i)))
            |> Array.map (fun x -> concentration_load ~p:(p (x)) ~l ~x)
            |> Array.to_list
            |> sum

    end

module ConcentrationLoad = 
    struct
        type t = {
            l  : float;
            p1 : float;
            x1 : float
        }

        let create ~l ~p1 ~x1 = { l; p1; x1 }
            

        let to_cmq (t) = 
            Cmq.concentration_load ~l:t.l ~p:t.p1 ~x:t.x1 

    end

module DistributionLoad = 
    struct 
        type t = {
            l  : float;
            p1 : float;
            p2 : float;
            x1 : float;
            x2 : float;
        }
        
        let create ~l ~p1 ~p2 ~x1 ~x2 = { l; p1; x1; p2; x2 }

        let to_cmq (t) = 
            if t.x1 = t.x2 
            then Cmq.empty ()
            else
                let f (x) = t.p1 +. (x -. t.x1) *. (t.p2 -. t.p1) /. (t.x2 -. t.x1) in
                Cmq.integration ~l:t.l ~f:f (t.x1, t.x2)

    end
