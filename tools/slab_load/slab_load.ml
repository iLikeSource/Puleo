(* =============================
    スラブ荷重の計算モジュール


   ============================= *)

open Algebra

module SlabLoad = 
    struct

        (** スラブ構成点 *)
        type slab_nodes = Point.t array

        type t = {
            slab_nodes    : slab_nodes;  (* スラブ構成点 *)
            load_par_area : float        (* 単位面積あたりの荷重 *)
        }

        (** 線分を取得 *)
        let get_lines (t) =
            let point_count = Array.length (t.slab_nodes) in
            t.slab_nodes 
            |> Array.mapi (fun i (x, y) ->
                if i = point_count - 1
                then ((x, y), t.slab_nodes.(0))
                else ((x, y), t.slab_nodes.(i + 1))
            ) 
            

        (** 隣接する線分の2等分線を取得 *)
        (*
        let get_neighbor_bisector (lines:line array) = 
            lines
            |> Array.mapi (fun i ((x1, y1), (x2, y2)) ->
                if i = point_count - 1 then
                    ((x, y), lines.(0))
                else 
                    ((x, y), lines.(i + 1))
            )
        *)
    end




