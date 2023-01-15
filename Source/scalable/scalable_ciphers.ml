(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
 *)
let key p q =
  let rec key_rec keys =
    if gcd_b keys (diff_b p [0;1]) = [0;1] && gcd_b keys (diff_b q [0;1]) = [0;1] then
      keys
    else
      key_rec (add_b keys [0;1])
  in key_rec [0;0;1];;


let generate_keys_rsa p q =
  let n = mult_b p q in
  let v = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in
  let e = key p q in
  match (bezout_b e v) with
    (u,_,_) -> ((n,e),(n,mod_b u v));;


(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p =
  let q = quot_b (diff_b p [0;1]) [0;0;1] in
  let g = add_b (from_int (Random.int(to_int q))) [0;1] in
  if (mod_power g [0;1] p = mod_power g (add_b q [0;1]) p) then
    (g,p)
  else
    public_data_g p;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = from_int (Random.int(((to_int p)-1)/2)) in
  let key_pub = mod_power g x p
  in (key_pub, x);;
(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let r = from_int (Random.int(((to_int p)-1)/2)) in
  let c = mod_power g r p in
  let d = mod_b (mult_b msg (mod_power kA r p)) p
  in (c,d);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)

let inv a b =
  match bezout_b a b with
    (u,_,_) -> u

let decrypt_g (msgA, msgB) a (g, p) =
  let c = mod_b (inv msgB p) p in
  let result = mod_b (mult_b (mod_power c a p) msgA) p in
  result;;
