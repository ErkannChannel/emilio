(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(*
#use "builtin.ml";;
#use "basic_arithmetics.ml";;*)

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec crt_image_rec list =
  match list with
    [] -> []
  | a::b -> modulo x a::crt_image_rec b
  in crt_image_rec l;;




(** Inverse image of Chinese Remainder map
    @para m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)

let inv f g =
  match bezout f g with
    (u,v,t) -> if u>v then
                 u
               else
                 v;;



let crt_solver m l y =
  let rec crt_resolver_rec l y =
    match (l,y) with
    | (a::b,c::d) -> let x = m/a in
                     let bez = inv a x in
                     c*x*bez+crt_resolver_rec b d
    | _ -> 0
  in (crt_resolver_rec l y) mod m;;




