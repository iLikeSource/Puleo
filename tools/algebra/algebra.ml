
module Point = 
    struct 
        type t = float * float
        

    end
;;

module Vector = 
    struct 
        type t = float * float

        let pi = 4.0 *. atan (1.0)
        
        (** “ñ“™•ªü *)
        let bisector (v1:t) (v2:t) = 
            let (vx1, vy1) = v1 in
            let (vx2, vy2) = v2 in
            let len1 = sqrt (vx1 ** 2.0 +. vy1 ** 2.0) in
            let len2 = sqrt (vx2 ** 2.0 +. vy2 ** 2.0) in
            let cos_t = (vx1 *. vx2 +. vy1 *. vy2) /. (len1 *. len2) in
            let theta = acos (cos_t) in
            let (vx', vy') = 
                let sin_t = sin (0.5 *. theta) in
                let cos_t = cos (0.5 *. theta) in
                ((cos_t *. vx1 -. sin_t *. vy1),
                 (sin_t *. vx1 +. cos_t *. vy1))
            in
            (vx', vy')
        

    end
;;
    

module LineSegment = 
    struct
        type t = {
            intercept : float option;
            gradient  : float option
        }

        let to_line (x1, y1) (x2, y2) = 
            let gradient = 
                if x1 = x2 
                then None
                else Some ((y2 -. y1) /. (x2 -. x1)) 
            in
            let intercept = 
                match gradient with
                | None        -> None
                | Some (grad) -> Some (y1 -. grad *. x1)
            in
            { gradient; intercept }
        
        let bind (l:t) = 
            match ((l.gradient, l.intercept)) with
            | (Some (g), Some (i)) -> Some (g, i)
            | (_       , _       ) -> None


        (** Œð“_ *)
        let cross_point (l1:t) (l2:t) = 
            if l1.gradient = l2.gradient then 
                None
            else (
                match (bind (l1), bind (l2)) with
                | (Some (g1, i1)), (Some (g2, i2)) ->
                    let x = (i2 -. i1) /. (g1 -. g2) in
                    let y = g1 *. x +. i1 in
                    Some (x, y)
                | (_, _) ->
                    None
            )

    end
;;
