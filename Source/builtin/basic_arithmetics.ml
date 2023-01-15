open Builtin


let rec gcd a b =
  if a = 0 then
    b
  else if b = 0 then
    a
  else
    let r = modulo a b  in
       gcd r a;;


let rec bezout a b =
  let rec bezout_rec a b u v u' v' =
    if b = 0 then
      (u,v,a)
    else
      let q = a/b in
      bezout_rec b (a - q * b) u' v' (u - q * u') (v - q * v')
  in bezout_rec a b 1 0 0 1;;


