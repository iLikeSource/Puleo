
module Point = 
    struct 
        type t = float * float
        

    end
;;

module Vector = 
    struct 
        type t = float * float

        let pi = 4.0 *. atan (1.0)
        
        let angle_rad (x, y) = 
            if x = 0.0 then (
                if y >= 0.0 then 
                    +. 0.5 *. pi 
                else 
                    -. 0.5 *. pi
            )
            else (
                (* atanは -π/2 ～ π/2 の範囲 *)
                if y >= 0.0 then 
                    if x >= 0.0 
                    then atan (y /. x)
                    else pi +. atan (y /. x)
                else 
                    if x >= 0.0 
                    then 2.0 *. pi +. atan (y /. x)
                    else        pi +. atan (y /. x) 
            )


        (** 二等分線 *)
        let bisector (v1:t) (v2:t) = 
            let (vx1, vy1) = v1 in
            let (vx2, vy2) = v2 in
            let len1 = sqrt (vx1 ** 2.0 +. vy1 ** 2.0) in
            let len2 = sqrt (vx2 ** 2.0 +. vy2 ** 2.0) in
            let _2theta = 
                let theta1 = angle_rad (vx1, vy1) in 
                let theta2 = angle_rad (vx2, vy2) in 
                if theta1 <= theta2 then
                    theta2 -. theta1
                else
                    2.0 *. pi +. theta2 -. theta1 
            in
            let cos_2theta = (vx1 *. vx2 +. vy1 *. vy2) /. (len1 *. len2) in
            let sin_theta = (0.5 *. (1.0 -. cos_2theta)) ** 0.5 in
            let cos_theta = 
                if _2theta <= pi then
                    (0.5 *. (1.0 +. cos_2theta)) ** 0.5 
                else
                    (0.5 *. (1.0 +. cos_2theta)) ** 0.5 *. (-1.0)
            in

            let (vx', vy') = 
                ((cos_theta *. vx1 -. sin_theta *. vy1),
                 (sin_theta *. vx1 +. cos_theta *. vy1))
            in
            (vx', vy')

            (*
            let (theta_min, theta_max) = 
                let theta1 = angle_rad (vx1, vy1) in 
                let theta2 = angle_rad (vx2, vy2) in 
                if theta1 < theta2 
                then (theta1, theta2)
                else (theta2, theta1)
            in
            let theta =
                (* acosは 0～πの範囲を返すため、
                   2つのベクトルの間に入る角度を選択する *)
                let theta_tmp = acos (cos_t) in
                let theta0 = 
                    if (theta_min +. theta_tmp) < 2.0 *. pi then
                        theta_min +. theta_tmp
                    else
                        theta_min +. theta_tmp -. 2.0 *. pi
                in
                if theta_min <= theta0 && theta0 <= theta_max then
                    theta_tmp
                else 
                    2.0 *. pi -. theta_tmp  
            in

            let (vx', vy') = 
                let sin_t = sin (0.5 *. theta) in
                let cos_t = cos (0.5 *. theta) in
                ((cos_t *. vx1 -. sin_t *. vy1),
                 (sin_t *. vx1 +. cos_t *. vy1))
            in
            (vx', vy')
            *) 

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


        (** 交点 *)
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
