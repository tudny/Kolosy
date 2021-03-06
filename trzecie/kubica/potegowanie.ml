(* Proste potęgowanie iteracyjne bez rekurencji ogonowej *)
let rec potega b n = 
  if n = 0 then 1 else b * potega b (n-1);;

(* Proste potęgowanie iteracyjne z rekuręcją ogonową *)
let potega b n = 
  let rec iter n a = 
    if n = 0 then a else iter (n - 1) (a * b)
  in
    iter n 1;;

(* Potęgowanie przez parzystość potęgi *)
let parzyste n = n mod 2 = 0;;
let square x = x * x;;
let potega b n = 
  let rec pot b n a = 
    if n = 0 then a 
    else if parzyste n then pot (square b) (n / 2) a
    else pot (square b) ((n - 1)/2) (a * b)
  in
    pot b n 1;;

assert (potega 2 2 = 4);;
assert (potega 5 0 = 1);;
assert (potega 3 4 = 81);;

