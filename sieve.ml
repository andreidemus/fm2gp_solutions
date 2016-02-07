(* Sieve of Eratosthenes. Draft *)

open Core.Std

let odd x = (x land 0b1) = 1
let even x = not (odd x)
			     
let val_of_index x = x + x + 3

let index_of_val x = (x - 3) / 2

let sieve_size m = m / 2 - 1 + (if odd m then 1 else 0)

let create_sieve m = Array.create ~len:(sieve_size m) true

let mark_sieve sieve x =
  let i = ref (index_of_val (x * x)) in
  while !i < (Array.length sieve) do
    sieve.(!i) <- false;
    i := !i + x;
  done;;

let print_sieve sieve =
  for i = 0 to Array.length sieve - 1 do
    if sieve.(i) then
      begin
	val_of_index i |> print_int;
	print_string "; ";
      end
    else ();
  done;
  print_string "\n";;
    
let process m =
  let sieve = create_sieve m in
  let x = ref 3 in
  while (!x * !x) < m do
    let i = index_of_val !x in
    if sieve.(i) then mark_sieve sieve !x else ();
    x := !x + 2;
  done;
  print_sieve sieve;;
    
let () = process 53;;
