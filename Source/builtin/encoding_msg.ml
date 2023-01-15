(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(*#use "power.ml";;*)

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)



let encode str bits =
  let rec encode_rec i compt =
    match i with
      i when i = (-1) -> 0
    | i -> int_of_char(str.[i])*(power 2 compt)+encode_rec (i-1) (compt+bits)
in encode_rec (String.length(str)-1) 0;;


let decode msg bits =
  let rec decode_rec word msg =
    match msg with
      0 -> word
    | msg -> decode_rec (Char.escaped(char_of_int(modulo msg (power 2 bits)))^word) (msg/(power 2 bits))
in decode_rec "" msg;;


