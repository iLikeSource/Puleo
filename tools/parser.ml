
module Attr = Map.Make (String)

let regexp_space = Str.regexp " +" 
let regexp_colon = Str.regexp ":" 

let read_attr (attrs:string list) = 
    attrs
    |> List.map (fun token -> Str.split regexp_colon token)
    |> List.map (fun pair -> (List.nth pair 0, List.nth pair 1))
    |> List.map (fun (k, v) -> (k, Scanf.sscanf v "%f" (fun x -> x)))
    |> List.fold_left (fun m (k, v) -> m |> Attr.add k v) Attr.empty

let split_cmnd_attrs (s:string) = 
    match Str.split regexp_space s with
    | cmnd_name::attrs -> (cmnd_name, attrs)
    | _ -> ("", [])

