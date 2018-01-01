module SHA1 = Hash.Make0(Digestif.SHA1)
module Hash = Hash.Make1(SHA1)
module Univ = Univ.Make(SHA1)

module Commit =
struct
  type t =
    { name: string
    ; time: int64
    ; parents: SHA1.t list }

  let univ =
    let hash { name; time; parents; } =
      let str = Fmt.strf "%Ld\000%a\000\000%s"
          time Fmt.(list ~sep:(const string "\000") (using SHA1.to_hex string)) parents name in
      SHA1.hash str
    in Univ.create ~name:"commit" hash
end

module Set =
struct
  include Set.Make(SHA1)

  let extend = add
  let exists = mem
end

module MMU =
struct

  type location = SHA1.t
  type 'v t = 'v -> location
end

let c1 =
  Commit.{ name = "c1"
         ; time = 0L
         ; parents = [] }
let c2 =
  Commit.{ name = "c2"
         ; time = 1L
         ; parents = [ Univ.hash Commit.univ c1 ] }
let c3 =
  Commit.{ name = "c3"
         ; time = 2L
         ; parents = [ Univ.hash Commit.univ c1 ] }
let c4 =
  Commit.{ name = "c4"
         ; time = 4L
         ; parents = [ Univ.hash Commit.univ c2
                     ; Univ.hash Commit.univ c3 ] }
let c5 =
  Commit.{ name = "c5"
         ; time = 3L
         ; parents = [ Univ.hash Commit.univ c3 ] }
let c6 =
  Commit.{ name = "c6"
         ; time = 5L
         ; parents = [ Univ.hash Commit.univ c4
                     ; Univ.hash Commit.univ c5 ] }
let c7 =
  Commit.{ name = "c7"
         ; time = 6L
         ; parents = [ Univ.hash Commit.univ c5 ] }

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = SHA1.t * Commit.t

      let compare (x, _) (y, _) = SHA1.compare x y
      let hash = Hashtbl.hash
      let equal (x, _) (y, _) = SHA1.eq x y
    end)
    (struct
      type t = string

      let default = ""
      let compare = String.compare
    end)

module Dot =
  Graph.Graphviz.Dot
    (struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_name (_, { Commit.name; _ }) = name
      let vertex_attributes _ = [ `Shape `Doublecircle ]
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes (_, l, _) = match l with
        | "" -> []
        | _ -> [ `Label l ]
    end)

module Pref = Pref.Make(MMU)(SHA1)(Set)

let store =
  let store = Pref.empty
      ~mmu:(Hash.of_type Commit.univ)
      () in
  let _, store = Pref.make c1 store in
  let _, store = Pref.make c2 store in
  let _, store = Pref.make c3 store in
  let _, store = Pref.make c4 store in
  let _, store = Pref.make c5 store in
  let _, store = Pref.make c6 store in
  let _, store = Pref.make c7 store in
  store

let of_store store =
  let graph = G.create () in
  Pref.iter
    (fun ({ Commit.name; time; parents; } as c) ->
       let hash_c = Univ.hash Commit.univ c in

       G.add_vertex graph (hash_c, c);
       List.iter (fun hash_p ->
           let p = Pref.get hash_p store in
           G.add_edge_e graph ((hash_c, c), "", (hash_p, p)))
         parents)
    store;
  graph

let to_dot ppf store =
  let graph = of_store store in
  Dot.fprint_graph ppf graph

module Node =
struct

  type t =
    { commit: Commit.t
    ; mutable color: [ `Black | `White ] }

  let univ =
    let hash { commit; _ } = Univ.hash Commit.univ commit in
    Univ.create ~name:"node" hash

  let compare b a =
    Int64.compare a.commit.Commit.time b.commit.Commit.time
end

module Pq = Psq.Make(SHA1)(Node)

let algorithm get exclude source =
  let s = Hashtbl.create 16 in

  let memoize get =
    fun h ->
      try Hashtbl.find s h
      with Not_found ->
        let c = get h in
        let n = { Node.commit = c; color = `White } in
        Hashtbl.add s h n;
        n in

  let get = memoize get in

  let all_blacks q =
    Pq.fold (fun _ -> function
        | { Node.color = `Black; _ } -> (&&) true
        | _ -> (&&) false) true q in

  let propagate { Node.commit; color } =
    let rec go q = match Q.shift q with
      | h, q ->
        (try let n = Hashtbl.find s h in
           n.Node.color <- color;
           go (List.fold_left Q.push q n.Node.commit.Commit.parents)
         with Not_found -> go q)
      | exception Q.Empty -> () in
    go (Q.of_list commit.Commit.parents) in

  let rec garbage q =
    if all_blacks q
    then ()
    else match Pq.pop q with
      | Some ((h, { Node.commit; color = `Black; }), q) ->
        List.fold_left
          (fun q h ->
             match get h with
             | { Node.color = `White; _ } as n ->
               n.Node.color <- `Black;
               propagate n;
               Pq.add h n q
             | n -> Pq.add h n q)
          q commit.Commit.parents
        |> garbage
      | Some ((h, n), q) ->
        List.fold_left
          (fun q h -> let n = get h in Pq.add h n q)
          q n.Node.commit.Commit.parents
        |> garbage
      | None -> ()
  in

  let collect () =
    Hashtbl.fold
      (fun k -> function
         | { Node.color = `White; _ } -> fun acc -> k :: acc
         | _ -> fun acc -> acc)
      s []
  in

  let q =
    List.append
      (List.map (fun k -> let n = get k in n.Node.color <- `Black; k, n) exclude)
      (List.map (fun k -> k, get k) source)
    |> Pq.of_list
  in

  garbage q;
  collect ()

let () =
  let l = algorithm (fun h -> Pref.get h store)
      [ Univ.hash Commit.univ c7
      ; Univ.hash Commit.univ c1 ]
      [ Univ.hash Commit.univ c6 ] in
  let l = List.map (fun h -> h, Pref.get h store) l in
  Fmt.(pf stdout) "%a.\n%!"
    Fmt.(Dump.list (Dump.pair SHA1.pp (using (fun { Commit.name; _ } -> name) string))) l
