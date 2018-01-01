module Eq =
struct

  type (_, _) refl = Refl : ('a, 'a) refl
end

module type HASH =
sig

  type t

  val hash: string -> t
end

module type S =
sig

  type hash
  type 'a t

  val hash: 'a t -> 'a -> hash
  val create: name:string -> ('a -> hash) -> 'a t
  val uid: 'a t -> int
  val name: 'a t -> string
  val same: 'a t -> 'b t -> bool
  val equal: 'a t -> 'b t -> ('a, 'b) Eq.refl option
  val equal_exn: 'a t -> 'b t -> ('a, 'b) Eq.refl
end

module Make (H: HASH): S with type hash = H.t =
struct

  type hash = H.t

  module Witness =
  struct

    module Key =
    struct
      type _ t = ..

      let hash _ t =
        H.hash (Fmt.strf "%d" Obj.(extension_id (extension_constructor t)))
    end

    module type S =
    sig
      type t
      type _ Key.t += Key: t Key.t
    end

    type 'a t = (module S with type t = 'a)

    let ok = Some Eq.Refl

    let equal (type u) (type v)
      : u t -> v t -> (u, v) Eq.refl option
      = fun (module U) (module V) ->
        match U.Key with
        | V.Key -> ok
        | _ -> None

    let equal_exn a b =
      match equal a b with
      | Some (_ as refl) -> refl
      | None -> invalid_arg "equal_exn"

    let hash (type a) hash (module M: S with type t = a) =
      Key.hash hash M.Key

    let create (type u) () =
      let module K = struct
        type t = u
        type _ Key.t += Key: t Key.t
      end in (module K: S with type t = u)

    let uid (type u) (module M: S with type t = u) =
      Obj.(extension_id (extension_constructor M.Key))
  end

  type 'a t =
    { witness : 'a Witness.t
    ; name    : string
    ; hash    : 'a -> hash }

  let equal a b = Witness.equal a.witness b.witness
  let equal_exn a b = Witness.equal_exn a.witness b.witness

  let hash : 'a t -> 'a -> hash = fun x -> x.hash

  let create ~name hash =
    { witness = Witness.create ()
    ; name
    ; hash }

  let uid { witness; _ } = Witness.uid witness
  let name { name; _ } = name

  let same a b =
    match Witness.equal a.witness b.witness with
    | Some _ -> true
    | None -> false
end
