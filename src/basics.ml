let abs x =
  if x >= 0 then x else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)
let rev_tup (a, b) = (b, a)

let rev_triple (a, b, c) = (c, b, a)

let is_odd x = x mod 2 <> 0

let is_older (y1, m1, d1) (y2, m2, d2) =
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

let to_us_format (y, m, d) = (m, d, y)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p =
  if p = 0 then 1 else x * pow x (p - 1)

let rec fac n =
  if n <= 1 then 1 else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth ((idx:int), (lst:'a list)) =
  match idx, lst with
  | 0, x :: _ -> x
  | n, _ :: t when n > 0 -> get_nth (n - 1, t)
  | _ -> failwith "index out of bounds"

let larger lst1 lst2 =
  let rec len = function [] -> 0 | _ :: t -> 1 + len t in
  let l1, l2 = len lst1, len lst2 in
  if l1 > l2 then lst1 else if l2 > l1 then lst2 else []

let sum lst1 lst2 =
  let rec s = function [] -> 0 | h :: t -> h + s t in
  s lst1 + s lst2