module type T =
sig

  type hash
  type 'a univ
  type 'a t = 'a -> hash

  val of_type: 'a univ -> 'a t
end

module type S =
sig
  type t

  val get: t -> int -> char
  val length: t -> int
  val eq: t -> t -> bool
  val neq: t -> t -> bool
  val compare: t -> t -> int
  val hash: string -> t
  val pp: t Fmt.t
  val to_hex: t -> string
  val of_hex: string -> t
end

module Make0 (H: Digestif_sig.S): S with type t = H.Bytes.t =
struct

  type t = H.Bytes.t

  let get hash idx =
    Bytes.get hash idx

  let length _ = H.digest_size

  exception Jump

  let eq x y =
    try
      for i = 0 to H.digest_size - 1
      do if not (Char.equal (Bytes.unsafe_get x i) (Bytes.unsafe_get y i))
        then raise Jump
      done; true
    with Jump -> false

  let neq x y = not (eq x y)

  let compare x y =
    let ret = ref 0 in
    try
      for i = 0 to H.digest_size - 1
      do ret := (Char.code (Bytes.unsafe_get x i))
                - (Char.code (Bytes.unsafe_get y i));

        if !ret <> 0 then raise Jump
      done; !ret
    with Jump -> !ret

  let hash x =
    H.Bytes.digest (Bytes.unsafe_of_string x)

  let pp = H.Bytes.pp

  let to_hex x = Bytes.unsafe_to_string (H.Bytes.to_hex x)
  let of_hex x = H.Bytes.of_hex (Bytes.unsafe_of_string x)
end

module Make1 (H: S): T
  with type hash = H.t
   and type 'a univ = 'a Univ.Make(H).t =
struct

  module Univ = Univ.Make(H)

  type hash = H.t
  type 'a univ = 'a Univ.t
  type 'a t = 'a -> hash

  let of_type (type a) (key: a Univ.t) = Univ.hash key
end


