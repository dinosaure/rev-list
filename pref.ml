module type DOMAIN =
sig
  type t
  type elt

  val empty: t
  val extend: elt -> t -> t
  val exists: elt -> t -> bool
end

module type LOCATION =
sig
  type t

  val get: t -> int -> char
  val length: t -> int
  val eq: t -> t -> bool
  val neq: t -> t -> bool
  val compare: t -> t -> int
end

module type MMU =
sig
  (** Memory Management Unit. This module implements a process to get
      an uniq {!location} from a value ['v]. *)

  type location
  type 'v t = 'v -> location
end

module type S =
sig
  (** This module implements a persistent store: in other words, it is
      a purely functional implementation of references, with an explicit
      store. *)

  type location
  type domain

  module MMU: MMU

  type 'v store

  val poly: unit -> 'v -> int
  (** Create a new MMU instance which is specialized with [int] (can
      be used only if [MMU.location = int]). *)

  val empty: mmu:'v MMU.t -> unit -> 'v store
  (** An empty store. *)

  val make: 'v -> 'v store -> location * 'v store
  (** Allocation. *)

  val get: location -> 'v store -> 'v
  (** Read access. *)

  val set: location -> 'v -> 'v store -> 'v store
  (** Write access. *)

  val iter: ('v -> unit) -> 'v store -> unit
  (** Iterating on all locations. *)

  val fold: ('acc -> location -> 'v -> 'acc) -> 'acc -> 'v store -> 'acc
  (** Folding. *)
end

module Make
    (M: MMU)
    (L: LOCATION with type t = M.location)
    (D: DOMAIN with type elt = M.location)
  : S
    with module MMU = M
     and type location = L.t
     and type domain = D.t =
struct

  type location = L.t
  type domain = D.t

  module MMU = M
  module Map = Radix.Make(L)

  type 'v store =
    { domain : D.t
    ; store  : 'v Map.t
    ; mmu    : 'v MMU.t }

  let poly () =
    let location = ref 0 in
    fun _ -> let res = !location in incr location; res

  let empty ~mmu () =
    { domain = D.empty
    ; store = Map.empty
    ; mmu }

  let make v { domain; store; mmu; } =
    let location = mmu v in
    location, { domain = D.extend location domain
              ; store = Map.add location v store
              ; mmu }

  let get location t =
    assert (D.exists location t.domain);

    match Map.lookup location t.store with
    | Some value -> value
    | None -> assert false

  let set location value t =
    assert (D.exists location t.domain);

    { t with store = Map.add location value t.store }

  let eq = L.eq
  let neq x y = L.neq
  let compare x y =
    L.compare x y

  let iter f t =
    Map.iter (fun _ v -> f v) t.store

  let fold f acc t =
    Map.fold (fun k v acc -> f acc k v) t.store acc
end
