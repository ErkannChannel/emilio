
(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(*#use "basic_arithmetics.ml";;
#use "power.ml";;*)

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  let rec encrypt_cesar_rec m =
    match m with
      [] -> []
     |a::l -> (modulo (a+k) b)::encrypt_cesar_rec l
  in encrypt_cesar_rec m;;


(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)



let decrypt_cesar k m b =
let rec decrypt_cesar_rec m =
match m with
[] -> []
|a::l -> (modulo (a-k) b)::decrypt_cesar_rec l
in decrypt_cesar_rec m;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
 *)
let key p q =
  let rec key_rec keys =
    if gcd keys (p-1)  = 1 && gcd keys (q-1) = 1 then
      keys
    else
      key_rec (keys+1)
  in key_rec 2;;



let generate_keys_rsa p q =
  let n = p * q in
  let v = (p-1)*(q-1) in
  let e = key p q in
  match (bezout e v) with
    (u,_,_) -> ((n,e),(n,modulo u v));;


(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;

(********** ElGamal Cipher **********)



(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)

let rec public_data_g p =
  let q = (p-1)/2 in
  let g = Random.int(q)+1 in
  if (mod_power g 1 p = mod_power g (q+1) p) then
    (g,p)
  else
    public_data_g p;;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let x = Random.int((p-1)/2)+2 in
  let key_pub = mod_power g x p
              in (key_pub, x);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let r = Random.int((p-1)/2) in
  let c = mod_power g r p in
  let d = modulo (msg*(mod_power kA r p)) p
        in (c,d);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let inv a b =
  match bezout a b with
    (u,_,_) -> u

let decrypt_g (msgA, msgB) a (g, p) =
  let c = modulo (inv msgB p) p in
  let result = modulo ((mod_power c a p)*msgA) p in
  result;;
