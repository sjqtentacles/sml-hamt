# sml-hamt

[![CI](https://github.com/sjqtentacles/sml-hamt/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-hamt/actions/workflows/ci.yml)

Persistent, immutable data structures for Standard ML built on a 32-ary
(5-bits-per-level) trie:

- **`HamtMap`** -- a hash array mapped trie (HAMT/CHAMP-style) map, keyed by a
  user-supplied hash and equality so it works for any key type.
- **`PVec`** -- a persistent bit-partitioned vector (Clojure-style 32-ary
  radix tree) with an append tail.

Every operation returns a new value and never mutates its input, so old
versions stay valid and share structure with their descendants. Everything is
pure Standard ML over the Basis library: deterministic, with no FFI, threads,
or clocks. The suite produces identical results on **MLton** and **Poly/ML**.

## Design

`HamtMap` distributes keys by hash, consuming 5 bits per level. An internal
node carries a 32-bit bitmap marking occupied slots plus a *packed* array of
children, so a node holding `k` entries uses an array of length `k` (not 32).
A child is a leaf entry, a hash-collision bucket, or another internal node.
Keys that share a full 32-bit hash are stored together in a collision node, so
**any** hash -- including a deliberately weak one -- yields a correct map; only
performance, never correctness, depends on hash quality.

`PVec` indexes elements by 5 bits per level through a radix tree of 32-wide
nodes, with a 32-element tail buffer for amortized O(1) `push`. `sub` and
`update` are O(log32 n). Because the trie is immutable, `push`/`update` copy
only the path from the root to the affected node and share the rest.

## API

```sml
structure HamtMap : sig
  type ('k, 'v) t
  val empty    : {hash : 'k -> word, eq : 'k * 'k -> bool} -> ('k, 'v) t
  val isEmpty  : ('k, 'v) t -> bool
  val size     : ('k, 'v) t -> int
  val insert   : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
  val find     : ('k, 'v) t -> 'k -> 'v option
  val contains : ('k, 'v) t -> 'k -> bool
  val remove   : ('k, 'v) t -> 'k -> ('k, 'v) t
  val foldl    : ('k * 'v * 'acc -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc
  val toList   : ('k, 'v) t -> ('k * 'v) list
end

structure PVec : sig
  type 'a t
  val empty    : 'a t
  val isEmpty  : 'a t -> bool
  val length   : 'a t -> int
  val push     : 'a t -> 'a -> 'a t
  val sub      : 'a t -> int -> 'a          (* raises Subscript if out of range *)
  val update   : 'a t -> int -> 'a -> 'a t  (* raises Subscript if out of range *)
  val foldl    : ('a * 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val toList   : 'a t -> 'a list
  val fromList : 'a list -> 'a t
end
```

### Example

```sml
val m0 = HamtMap.empty {hash = hashString, eq = (op =) : string * string -> bool}
val m1 = HamtMap.insert m0 "apple" 1
val m2 = HamtMap.insert m1 "banana" 2
val m3 = HamtMap.insert m2 "banana" 200   (* new version *)
val _  = HamtMap.find m3 "banana"          (* SOME 200 *)
val _  = HamtMap.find m2 "banana"          (* SOME 2 -- m2 is untouched *)

val v0 = PVec.fromList [10, 20, 30]
val v1 = PVec.update (PVec.push v0 40) 1 999
val _  = PVec.toList v1                     (* [10, 999, 30, 40] *)
val _  = PVec.toList v0                     (* [10, 20, 30] -- unchanged *)
```

See [`examples/demo.sml`](examples/demo.sml); run it with `make example`.

## Build & test

Requires [MLton](http://mlton.org/) and/or [Poly/ML](https://polyml.org/).

```sh
make test        # build + run the suite under MLton
make test-poly   # run the suite under Poly/ML
make all-tests   # both
make example     # build + run the demo
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-hamt
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-hamt/hamt.mlb` from your own `.mlb`
(MLton / MLKit), or feed `sources.mlb` to `tools/polybuild` (Poly/ML).

## Layout

```
sml.pkg                                       smlpkg manifest
Makefile                                      MLton + Poly/ML targets
.github/workflows/ci.yml                      CI: MLton + Poly/ML
lib/github.com/sjqtentacles/sml-hamt/
  hamt.sig         HAMT_MAP + PVEC signatures
  hamt.sml         HamtMap (HAMT/CHAMP) + PVec (radix vector)
  sources.mlb      ordered source list
  hamt.mlb         public basis
test/
  harness.sml  shared assertion harness
  test.sml     oracle, collision, immutability, and vector suites
  entry.sml / main.sml
examples/demo.sml  map + vector persistence demo
tools/polybuild    Poly/ML build wrapper
```

## Tests

53 deterministic checks. The map is driven against an assoc-list oracle using
an LCG-generated sequence of insert/remove operations under three hashes -- a
mixing hash, a deliberately weak 8-bucket hash, and a single-bucket hash --
asserting `find`/`size`/`toList` agree at **every** step. Further suites cover
explicit collision-node handling, structural immutability (older versions are
unaffected by later updates), and `PVec` push/sub/update/order across several
trie levels. Run `make all-tests` to verify identical output under both
compilers.

## License

MIT. See [LICENSE](LICENSE).
