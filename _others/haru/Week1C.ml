(*
課題1
ocamlc -c strSet.mli
ocamlc -c strSet.ml
ocamlc -c sort.ml
ocamlc -o sort strSet.cmo sort.cmo
*)

(*課題2*)
module type STACK = sig
  type 'a t

  val pop : 'a t -> 'a * 'a t
  val push : 'a -> 'a t -> 'a t
  val empty : 'a t
  val size : 'a t -> int
end

module Stack : STACK = struct
  type 'a t = 'a list

  let pop s = match s with [] -> failwith "error" | h :: t -> (h, t)
  let push x l = x :: l
  let empty = []
  let size s = List.length s
end

(*課題3*)
type order = LT | EQ | GT

module type ORDERED_TYPE = sig
  type t

  val compare : t -> t -> order
end

module Set (Order : ORDERED_TYPE) : sig
  type t
  type elt = Order.t

  val empty : t
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val mem : elt -> t -> bool
  val size : t -> int
end = struct
  type elt = Order.t
  type t = Leaf | Node of elt * t * t

  let empty = Leaf

  let rec add x s =
    match s with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (y, l, r) -> (
        match Order.compare x y with
        | LT -> Node (y, add x l, r)
        | EQ -> s
        | GT -> Node (y, l, add x r))

  let rec remove x s =
    match s with
    | Leaf -> s
    | Node (y, l, r) when Order.compare x y = LT -> Node (y, remove x l, r)
    | Node (y, l, r) when Order.compare x y = GT -> Node (y, l, remove x r)
    | Node (y, l, Leaf) -> l
    | Node (y, Leaf, r) -> r
    | Node (y, Node (z, ll, lr), r) -> Node (z, remove z (Node (z, ll, lr)), r)

  let rec mem x t =
    match t with
    | Leaf -> false
    | Node (y, l, r) when Order.compare x y = LT -> mem x l
    | Node (y, l, r) when Order.compare x y = GT -> mem x r
    | _ -> true

  let rec size t =
    match t with Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
end

(*課題４*)
module Map (Order : ORDERED_TYPE) : sig
  type 'a t
  type key = Order.t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val get : key -> 'a t -> 'a option
  val size : 'a t -> int
end = struct
  type key = Order.t
  type 'a t = Leaf | Node of (key * 'a) * 'a t * 'a t

  let empty = Leaf

  let rec add x v s =
    match s with
    | Leaf -> Node ((x, v), Leaf, Leaf)
    | Node ((y, w), l, r) -> (
        match Order.compare x y with
        | LT -> Node ((y, w), add x v l, r)
        | EQ -> Node ((y, v), l, r)
        | GT -> Node ((y, w), l, add x v r))

  let rec remove x s =
    match s with
    | Leaf -> s
    | Node ((y, v), l, r) when Order.compare x y = LT ->
        Node ((y, v), remove x l, r)
    | Node ((y, v), l, r) when Order.compare x y = GT ->
        Node ((y, v), l, remove x r)
    | Node ((y, _), l, Leaf) -> l
    | Node ((y, _), Leaf, r) -> r
    | Node ((y, _), Node ((z, v), ll, lr), r) ->
        Node ((z, v), remove z (Node ((z, v), ll, lr)), r)

  let rec mem x t =
    match t with
    | Leaf -> false
    | Node ((y, _), l, r) when Order.compare x y = LT -> mem x l
    | Node ((y, _), l, r) when Order.compare x y = GT -> mem x r
    | _ -> true

  let rec size t =
    match t with Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r

  let rec get x s =
    match s with
    | Leaf -> None
    | Node ((y, _), l, r) when Order.compare x y = LT -> get x l
    | Node ((y, _), l, r) when Order.compare x y = GT -> get x r
    | Node ((_, v), _, _) -> Some v
end
