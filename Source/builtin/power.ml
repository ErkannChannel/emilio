
open Builtin
open Basic_arithmetics

(*
#use "basic_arithmetics.ml";;
#use "builtin.ml";;*)


let pow x n =
  let rec pow_rec n =
    if n <> 0 then
      x*(pow_rec (n-1))
    else
      1
    in pow_rec n;;

let power x n =
  if n = 0 then 1 else
  let rec power_rec n =
    if n = 1 then
      x
    else if n mod 2 = 0 then
      (power_rec (n/2))*(power_rec (n/2))
    else
      x*(power_rec (n/2))*(power_rec (n/2))
    in power_rec n;;



let mod_power x n m =
  if n = 0 then 1 else
  let result = modulo x m in
  let rec mod_power_rec n result =
    if n = 1 then
      result
    else
      mod_power_rec (n-1) (modulo (x*result) m)
  in mod_power_rec n result;;


let prime_mod_power x n p =
  if n = 0 then 1 else
    let result = modulo x p in
    let rec mod_power_rec n result =
      if n = 1 then
        result
      else
        mod_power_rec (n-1) (modulo (x*result) p)
    in mod_power_rec n result;;



