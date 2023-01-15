

let sign x =
  match x with
    x when x>=0 -> 1
  | _ -> -1;;



let quot a b =
  if a<0 && b<0 then
    (abs a)/(abs b)+1
  else
    if a<0 && b>0 && a mod b = 0 then
      (-1)*((abs a)/(abs b))
    else if a<0 && b>0 then
      (-1)*((abs a)/(abs b))-1
      else if a>0 && b<0 then
        (-1)*((abs a)/(abs b))
      else
        a/b;;



let modulo a b =
  if a<0 && b<0 &&  a mod b = 0 then
    0
  else
    if a<0 && b<0 then
      abs(b - (a mod b))
    else
      if a<0 && b>0 && a mod b <> 0 then
        (b + a mod b)
      else
        a mod b;;



let div a b =
  if a<0 && b>0 then
    (((quot a b)), (modulo a b))
  else if a<0 && b<0 then
    (((quot a b)), (modulo a b))
  else
    ((quot a b), (modulo a b));;

