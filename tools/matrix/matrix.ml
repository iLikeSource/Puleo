
module MatrixComponent =
    struct
        type t = int * float
        let multiply (a0:t list) (b0:t list) = 
            (* 行と列のインデックスが同一の箇所を乗算し和をとる *)
            let rec multiply_rec (dst0:float) (a:t list) (b:t list) = 
                match (a0, b0) with
                | ((a_idx, a_entry)::a_tl, (b_idx, b_entry)::b_tl) -> 
                    let (a1, b1, dst1) =  
                        if a_idx = b_idx then
                            (a_tl, b_tl, (dst0 +. a_entry *. b_entry))
                        else if a_idx < b_idx then
                            (a_tl, b0  , dst0)
                        else 
                            (a0  , b_tl, dst0)
                    in
                    multiply_rec dst1 a1 b1
                | (_, _) -> dst0
            in        
            (* 短い方を先にする *)
            let (a0, b0) = 
                if List.length a0 < List.length b0
                then (a0, b0)
                else (b0, a0)
            in
            multiply_rec 0.0 a0 b0

    end

module MatrixSource = 
    struct
        type t = int * int * float
            
    
    end


module type MatrixType = 
    sig
        type t 
        val get : t -> int * int -> float
        val set : t -> int * int * float -> unit
        val add : t -> int * int * float -> unit
        val rows : t -> MatrixComponent.t list array
        val columns : t -> MatrixComponent.t list array
        val row : t -> int -> MatrixComponent.t list
        val column : t -> int -> MatrixComponent.t list
        val to_sources : t -> MatrixSource.t list
        val of_sources : MatrixSource.t list -> t 
    end

(** immutable matrix *)
module Matrix(M: MatrixType) = 
    struct
        type t = M.t
        
        let add_mm (a: t) (b: t) = 
            [ M.to_sources (a); M.to_sources (b) ]
            |> List.concat
            |> M.of_sources
        
        let multiply_mm (a: t) (b: t) = 
            let a_rows    = M.rows (a) in
            let b_columns = M.columns (b) in
            a_rows
            |> Array.mapi (fun row a_row ->
                b_columns 
                |> Array.mapi (fun clm b_clm ->
                    MatrixComponent.multiply a_row b_clm
                )
            )

    end







