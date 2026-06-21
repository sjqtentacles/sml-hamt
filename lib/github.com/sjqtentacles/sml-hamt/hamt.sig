(* hamt.sig

   Persistent, immutable data structures for Standard ML built on a
   32-ary (5-bits-per-level) trie:

     HamtMap  a hash array mapped trie (HAMT/CHAMP-style) map, keyed by a
              user-supplied hash and equality so it works for any key type.
     PVec     a persistent bit-partitioned vector (32-ary radix tree).

   Every update returns a new value; the input is never mutated, so old
   versions remain valid and share structure with their descendants.
   Everything is pure Standard ML over the Basis library, deterministic,
   with no FFI, threads, or clocks. *)

(* A persistent hash map.

   Keys are compared by the {hash, eq} record supplied to `empty`. `hash`
   maps a key to a `word` (only the low 32 bits are consulted); `eq` is the
   key equality used within a slot. Distinct keys whose hashes collide are
   stored together in a collision node, so any `hash` -- including a
   deliberately weak one -- yields a correct map. *)
signature HAMT_MAP =
sig
  type ('k, 'v) t

  (* An empty map using the given hash and equality. *)
  val empty  : {hash : 'k -> word, eq : 'k * 'k -> bool} -> ('k, 'v) t

  (* True if the map has no bindings. *)
  val isEmpty : ('k, 'v) t -> bool

  (* Number of bindings. *)
  val size   : ('k, 'v) t -> int

  (* `insert m k v` is `m` with `k` bound to `v` (replacing any prior
     binding for `k`). `m` is unchanged. *)
  val insert : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t

  (* The value bound to `k`, or NONE. *)
  val find   : ('k, 'v) t -> 'k -> 'v option

  (* True if `k` is bound. *)
  val contains : ('k, 'v) t -> 'k -> bool

  (* `remove m k` is `m` without any binding for `k` (a no-op if `k` is
     absent). `m` is unchanged. *)
  val remove : ('k, 'v) t -> 'k -> ('k, 'v) t

  (* Left fold over all bindings. Iteration order is unspecified. *)
  val foldl  : ('k * 'v * 'acc -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc

  (* All bindings as a list, in unspecified order. *)
  val toList : ('k, 'v) t -> ('k * 'v) list
end

(* A persistent indexed sequence (bit-partitioned vector).

   Indices run from 0 to `length v - 1`. `sub` and `update` raise `Subscript`
   for out-of-range indices. `push` appends at the end. *)
signature PVEC =
sig
  type 'a t

  (* The empty vector. *)
  val empty  : 'a t

  (* True if the vector has no elements. *)
  val isEmpty : 'a t -> bool

  (* Number of elements. *)
  val length : 'a t -> int

  (* `push v x` is `v` with `x` appended; `v` is unchanged. *)
  val push   : 'a t -> 'a -> 'a t

  (* `sub v i` is the element at index `i`; raises `Subscript` if out of
     range. *)
  val sub    : 'a t -> int -> 'a

  (* `update v i x` is `v` with index `i` set to `x`; `v` is unchanged.
     Raises `Subscript` if `i` is out of range. *)
  val update : 'a t -> int -> 'a -> 'a t

  (* Left fold over elements in index order. *)
  val foldl  : ('a * 'acc -> 'acc) -> 'acc -> 'a t -> 'acc

  (* All elements as a list, in index order. *)
  val toList : 'a t -> 'a list

  (* Build a vector from a list, preserving order. *)
  val fromList : 'a list -> 'a t
end
