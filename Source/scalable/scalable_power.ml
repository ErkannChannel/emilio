(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics
(*
#use "scalable.ml";;
#use "scalable_basic_arithmetics";;*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)

let pow x n =
  let rec pow_rec n =
    if ((>>) n [0;0]) then
      mult_b x (pow_rec (diff_b n [0;1]))
    else
      [0;1]
  in pow_rec n;;




let power x n =
  if compare_b n [0;0] = 0 || x = [0] && n = [0] || n = [0] then
    [0;1]
  else
    let rec power_rec n =
      if compare_b n [0;1] = 0 then
        x
      else if compare_b (mod_b n [0;0;1]) [0] = 0 then
        mult_b (power_rec (quot_b n [0;0;1])) (power_rec (quot_b n [0;0;1]))
      else
        mult_b x (mult_b (power_rec (quot_b n [0;0;1])) (power_rec (quot_b n [0;0;1])))
      in power_rec n;;




(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)

let mod_power x n m =
  if n = [0] then
    [0;1]
  else
    let result = mod_b x m in
    let rec mod_power_rec n result =
      if n = [0;1] then
        result
      else
        mod_power_rec (diff_b n [0;1]) (mod_b (mult_b x result) m)
    in mod_power_rec n result;;

let prime_mod_power x n p =
  if n = [0] then
    [0;1]
  else
    let result = mod_b x p in
    let rec mod_power_rec n result =
      if n = [0;1] then
        result
      else
        mod_power_rec (diff_b n [0;1]) (mod_b (mult_b x result) p)
    in mod_power_rec n result;;

