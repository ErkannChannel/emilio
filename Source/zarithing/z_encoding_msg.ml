(** Encoding Strings *)

open Z
open Z_power



let encode str bits =
  let rec encode_rec i compt =
    match i with
      i when i = minus_one -> zero
    | i -> add (mul (of_int(int_of_char(str.[(to_int i)]))) (pow (of_int 2) (to_int compt))) (encode_rec (pred i) (add compt (of_int bits)))
in encode_rec (pred (of_int(String.length(str)))) zero;;



let decode msg bits =
  let rec decode_rec word msg =
    if msg = zero then
      word
    else
      decode_rec (Char.escaped(char_of_int(to_int(rem msg (pow (of_int 2) bits))))^word) (div msg (pow (of_int 2) bits))
  in decode_rec "" msg;;


