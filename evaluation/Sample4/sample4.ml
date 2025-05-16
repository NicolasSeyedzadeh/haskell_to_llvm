type 'a option =
  | None
  | Some of 'a

let bind f value = match value with
 | None -> None
 | Some r -> f r


let rec safediv x y = match y with
 | 0 -> None
 | y -> Some (x/y)


let rec ifNone x  = match x with
 | None -> print_string "error: divide by 0"
 | Some r -> print_int r

let () = ifNone (bind (safediv 25) (bind (safediv 40) (Some 8)))
