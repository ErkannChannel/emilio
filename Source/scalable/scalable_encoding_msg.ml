(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power


(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)


let encode str bits =
  let rec encode_rec i compt =
    match i with
      i when i = [1;1] -> [0]
    | i -> add_b (mult_b (from_int (int_of_char(str.[(to_int i)]))) (power [0;0;1] (from_int compt))) (encode_rec (diff_b i [0;1]) (compt+bits))
in encode_rec (diff_b (from_int (String.length(str))) [0;1]) 0;;


let decode msg bits =
  let rec decode_rec word msg =
    match msg with
      [0] -> word
    | msg -> decode_rec (Char.escaped(char_of_int(to_int(mod_b msg (power [0;1;1] (from_int bits)))))^word) (quot_b msg (power [0;0;1] (from_int bits)))
in decode_rec "" msg;;


