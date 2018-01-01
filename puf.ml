module Make
    (M: Pref.MMU)
    (L: Pref.LOCATION with type t = M.location)
    (D: Pref.DOMAIN with type elt = M.location) =
struct
  module Pref = Pref.Make(M)(L)(D)

  type point = L.t

  type 'a content =
    | Link of point
    | Root of 'a

  type 'v state =
    { mutable contents : 'v content Pref.store }

  let rec repr x ({ contents } as state) =
    match Pref.get x contents with
    | Root _ -> x
    | Link y ->
      let z = repr y state in
      if L.neq y z
      then state.contents <- Pref.set x (Pref.get y contents) contents;

      z

  let init ~mmu () =
    let mmu = function
      | Root x -> mmu x
      | _ -> assert false in
    { contents = Pref.empty ~mmu () }

  let create desc { contents } =
    let location, store = Pref.make (Root desc) contents in
    location, { contents = store }

  let same x y state =
    L.eq (repr x state) (repr y state)

  let union x y state =
    let rx = repr x state in
    let ry = repr y state in

    if L.eq rx ry
    then state
    else { contents = Pref.set rx (Link ry) state.contents }

  let find x ({ contents } as state) =
    match Pref.get x contents with
    | Root desc -> desc
    | Link y ->
      match Pref.get y contents with
      | Root desc -> desc
      | Link _ ->
        let r = repr x state in
        match Pref.get r contents with
        | Root desc -> desc
        | Link _ -> assert false

  let update f x ({ contents } as state) =
    let rx = repr x state in
    let descx = find rx state in
    { contents = Pref.set rx (Root (f descx)) contents }

  let iter f { contents } =
    Pref.iter
      (function
        | Link _ -> ()
        | Root desc -> f desc)
      contents

  let fold f acc { contents } =
    Pref.fold
      (fun acc k -> function
         | Link _ -> acc
         | Root desc -> f acc k desc)
      acc contents
end
