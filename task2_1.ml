open Core.Std

let divisor x =
  let rec divisor' x i =
    if x mod i = 0 then i
    else divisor' x (i + 1)
  in if x = 1 then 1
     else divisor' x 2;;

let rec terms x =
  let d = divisor x in
  match x with
  | 0 -> []
  | 1 -> [1]
  | x' when x' = d -> 1::(terms (x' - 1))
  | x' -> let rec terms' x t ts =
            let t' = t * 2 in
            if t' > x then ts else t::(terms' x t' ts) in
          terms' x' d [];;

let parts x =
  let rec parts' x ts res =
    if x = 0 then res
    else match ts with
	 | h::tl when h > x -> parts' x tl res
	 | h::_ -> parts' (x - h) ts (h::res)
	 | [] -> x::res
  in parts' x (terms x |> List.rev) [];;

let print_result_line (x,xs) =
  let s = List.map xs ~f:string_of_int |> String.concat ~sep:"; " in
  print_string ((string_of_int x) ^ ": [" ^ s ^ "]\n");;
  
let () =
  for i = 1 to 99 do
    print_result_line (i, parts i)
  done;;
  
  (* List.range 1 100 *)
  (* |> List.map ~f:(fun x -> (x, parts x)) *)
  (* |> List.iter ~f:print_result_line;; *)
