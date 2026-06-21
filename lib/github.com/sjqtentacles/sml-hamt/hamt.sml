(* hamt.sml -- persistent map (HAMT) and vector (32-ary radix tree).

   Both structures use a 32-way trie with 5 bits of index per level and
   immutable Basis `vector`s for the packed children, so each update copies
   only the spine from the root to the touched node and shares everything
   else.

   HamtMap is a hash array mapped trie: an internal node carries a 32-bit
   bitmap marking occupied slots plus a packed array of children (a child is
   a leaf entry, a hash-collision bucket, or another internal node). Keys
   that share a full hash land in a collision node, so any user hash --
   including a deliberately weak one -- is correct.

   PVec is a Clojure-style bit-partitioned vector with a 32-element tail for
   amortized O(1) append; `sub`/`update` are O(log32 n). *)

(* ---------------- shared 5-bit-per-level trie constants ---------------- *)
structure HamtBits =
struct
  val BITS  = 0w5 : word
  val WIDTH = 32  : int
  val MASK  = 0w31 : word

  (* Population count of the low 32 bits (enough for a 32-slot bitmap). *)
  fun popcount w =
    let
      val w = Word.- (w, Word.andb (Word.>> (w, 0w1), 0wx55555555))
      val w = Word.+ (Word.andb (w, 0wx33333333),
                      Word.andb (Word.>> (w, 0w2), 0wx33333333))
      val w = Word.andb (Word.+ (w, Word.>> (w, 0w4)), 0wx0f0f0f0f)
      val w = Word.+ (w, Word.>> (w, 0w8))
      val w = Word.+ (w, Word.>> (w, 0w16))
    in Word.toInt (Word.andb (w, 0wx7f)) end

  (* The 5-bit fragment of `h` at the given shift, as an int 0..31. *)
  fun frag (h, shift) = Word.toInt (Word.andb (Word.>> (h, shift), MASK))

  (* The single-bit mask for fragment `f`. *)
  fun bit f = Word.<< (0w1, Word.fromInt f)
end

(* ============================ HamtMap ============================ *)

structure HamtMap :> HAMT_MAP =
struct
  open HamtBits

  datatype ('k, 'v) node =
      Leaf      of word * 'k * 'v
    | Collision of word * ('k * 'v) list
    | Bitmap    of word * ('k, 'v) node vector

  type ('k, 'v) t =
    { hash : 'k -> word, eq : 'k * 'k -> bool,
      root : ('k, 'v) node, count : int }

  val empty32 : word = 0wxFFFFFFFF
  fun mask h = Word.andb (h, empty32)

  (* ---- packed-array helpers (immutable copies) ---- *)
  fun vInsertAt (vec, idx, x) =
    Vector.tabulate (Vector.length vec + 1,
      fn i => if i < idx then Vector.sub (vec, i)
              else if i = idx then x
              else Vector.sub (vec, i - 1))

  fun vRemoveAt (vec, idx) =
    Vector.tabulate (Vector.length vec - 1,
      fn i => if i < idx then Vector.sub (vec, i) else Vector.sub (vec, i + 1))

  fun vec1 x = Vector.fromList [x]
  fun vec2 (a, b) = Vector.fromList [a, b]

  (* Merge a single-hash subtree (Leaf/Collision with hash = nodeHash) and a
     new leaf whose hash differs, at the given shift. Terminates because two
     distinct 32-bit hashes differ in some 5-bit fragment at shift <= 30. *)
  fun mergeNodeLeaf (shift, nodeHash, node, (h, k, v)) =
    let
      val f1 = frag (nodeHash, shift)
      val f2 = frag (h, shift)
    in
      if f1 = f2 then
        Bitmap (bit f1, vec1 (mergeNodeLeaf (shift + BITS, nodeHash, node, (h, k, v))))
      else
        let
          val leaf = Leaf (h, k, v)
          val children = if f1 < f2 then vec2 (node, leaf) else vec2 (leaf, node)
        in Bitmap (Word.orb (bit f1, bit f2), children) end
    end

  (* ---- insert: returns (newNode, sizeDelta) ---- *)
  fun insertNode eq (node, shift, h, k, v) =
    case node of
        Leaf (h0, k0, v0) =>
          if eq (k0, k) then (Leaf (h, k, v), 0)
          else if h0 = h then (Collision (h, [(k0, v0), (k, v)]), 1)
          else (mergeNodeLeaf (shift, h0, node, (h, k, v)), 1)
      | Collision (h0, entries) =>
          if h = h0 then
            if List.exists (fn (k', _) => eq (k', k)) entries then
              (Collision (h0, List.map (fn (e as (k', _)) =>
                  if eq (k', k) then (k, v) else e) entries), 0)
            else (Collision (h0, (k, v) :: entries), 1)
          else (mergeNodeLeaf (shift, h0, node, (h, k, v)), 1)
      | Bitmap (bm, children) =>
          let
            val f = frag (h, shift)
            val b = bit f
            val idx = popcount (Word.andb (bm, b - 0w1))
          in
            if Word.andb (bm, b) = 0w0 then
              (Bitmap (Word.orb (bm, b), vInsertAt (children, idx, Leaf (h, k, v))), 1)
            else
              let
                val (child', delta) =
                  insertNode eq (Vector.sub (children, idx), shift + BITS, h, k, v)
              in (Bitmap (bm, Vector.update (children, idx, child')), delta) end
          end

  (* ---- find ---- *)
  fun findNode eq (node, shift, h, k) =
    case node of
        Leaf (_, k0, v0) => if eq (k0, k) then SOME v0 else NONE
      | Collision (h0, entries) =>
          if h = h0 then Option.map #2 (List.find (fn (k', _) => eq (k', k)) entries)
          else NONE
      | Bitmap (bm, children) =>
          let val b = bit (frag (h, shift)) in
            if Word.andb (bm, b) = 0w0 then NONE
            else findNode eq (Vector.sub (children, popcount (Word.andb (bm, b - 0w1))),
                              shift + BITS, h, k)
          end

  (* ---- remove: returns (newNodeOpt, sizeDelta); NONE = node now empty ---- *)
  fun removeNode eq (node, shift, h, k) =
    case node of
        Leaf (_, k0, _) =>
          if eq (k0, k) then (NONE, 1) else (SOME node, 0)
      | Collision (h0, entries) =>
          if h <> h0 then (SOME node, 0)
          else if List.exists (fn (k', _) => eq (k', k)) entries then
            let val rest = List.filter (fn (k', _) => not (eq (k', k))) entries in
              case rest of
                  [(k1, v1)] => (SOME (Leaf (h0, k1, v1)), 1)
                | _ => (SOME (Collision (h0, rest)), 1)
            end
          else (SOME node, 0)
      | Bitmap (bm, children) =>
          let
            val b = bit (frag (h, shift))
          in
            if Word.andb (bm, b) = 0w0 then (SOME node, 0)
            else
              let
                val idx = popcount (Word.andb (bm, b - 0w1))
                val (childOpt, delta) =
                  removeNode eq (Vector.sub (children, idx), shift + BITS, h, k)
              in
                case childOpt of
                    NONE =>
                      let val bm' = Word.andb (bm, Word.notb b) in
                        if bm' = 0w0 then (NONE, delta)
                        else (SOME (Bitmap (bm', vRemoveAt (children, idx))), delta)
                      end
                  | SOME child' =>
                      (SOME (Bitmap (bm, Vector.update (children, idx, child'))), delta)
              end
          end

  (* ---- public API ---- *)
  fun empty {hash, eq} =
    { hash = hash, eq = eq, root = Bitmap (0w0, Vector.fromList []), count = 0 }

  fun size (m : ('a, 'b) t) = #count m
  fun isEmpty (m : ('a, 'b) t) = #count m = 0

  fun insert (m : ('a, 'b) t) k v =
    let
      val h = mask (#hash m k)
      val (root', delta) = insertNode (#eq m) (#root m, 0w0, h, k, v)
    in
      { hash = #hash m, eq = #eq m, root = root', count = #count m + delta }
    end

  fun find (m : ('a, 'b) t) k =
    findNode (#eq m) (#root m, 0w0, mask (#hash m k), k)

  fun contains (m : ('a, 'b) t) k = Option.isSome (find m k)

  fun remove (m : ('a, 'b) t) k =
    let
      val h = mask (#hash m k)
      val (rootOpt, delta) = removeNode (#eq m) (#root m, 0w0, h, k)
      val root' = case rootOpt of
                      NONE => Bitmap (0w0, Vector.fromList [])
                    | SOME n => n
    in
      { hash = #hash m, eq = #eq m, root = root', count = #count m - delta }
    end

  fun foldl f acc (m : ('a, 'b) t) =
    let
      fun go (Leaf (_, k, v), a) = f (k, v, a)
        | go (Collision (_, es), a) = List.foldl (fn ((k, v), a) => f (k, v, a)) a es
        | go (Bitmap (_, ch), a) = Vector.foldl go a ch
    in go (#root m, acc) end

  fun toList m = foldl (fn (k, v, acc) => (k, v) :: acc) [] m
end

(* ============================== PVec ============================== *)

structure PVec :> PVEC =
struct
  open HamtBits

  datatype 'a node = Interior of 'a node vector | Values of 'a vector

  (* A nullary `Empty` keeps the public `empty` a polymorphic value (it would
     otherwise trip the value restriction, since a record holding
     `Vector.fromList []` is an expansive expression). *)
  datatype 'a t =
      Empty
    | Vec of { cnt : int, shift : word, root : 'a node, tail : 'a vector }

  val empty : 'a t = Empty

  fun unwrap Empty =
        { cnt = 0, shift = BITS, root = Interior (Vector.fromList []),
          tail = Vector.fromList [] }
    | unwrap (Vec r) = r

  fun length Empty = 0
    | length (Vec r) = #cnt r
  fun isEmpty Empty = true
    | isEmpty (Vec r) = #cnt r = 0

  (* Index of the first element held in the tail. *)
  fun tailoff cnt =
    if cnt < WIDTH then 0
    else Word.toInt (Word.<< (Word.>> (Word.fromInt (cnt - 1), BITS), BITS))

  fun idx (i, level) = Word.toInt (Word.andb (Word.>> (Word.fromInt i, level), MASK))

  fun vAppend (vec, x) =
    Vector.tabulate (Vector.length vec + 1,
      fn i => if i < Vector.length vec then Vector.sub (vec, i) else x)

  fun sub v i =
    let val {cnt, shift, root, tail} = unwrap v in
      if i < 0 orelse i >= cnt then raise Subscript
      else if i >= tailoff cnt then Vector.sub (tail, i - tailoff cnt)
      else
        let
          fun go (Interior arr, level) = go (Vector.sub (arr, idx (i, level)), level - BITS)
            | go (Values vals, _) = Vector.sub (vals, Word.toInt (Word.andb (Word.fromInt i, MASK)))
        in go (root, shift) end
    end

  (* Build a left spine of `level/5` interior nodes ending in `node`. *)
  fun newPath (level, node) =
    if level = 0w0 then node
    else Interior (Vector.fromList [newPath (level - BITS, node)])

  (* Insert the full tail (as a Values node) into the tree at `level`. *)
  fun pushTail (cnt, level, arr, tailNode) =
    let
      val subidx = idx (cnt - 1, level)
      val n = Vector.length arr
      val child =
        if level = BITS then tailNode
        else if subidx < n then
          (case Vector.sub (arr, subidx) of
               Interior childArr => pushTail (cnt, level - BITS, childArr, tailNode)
             | Values _ => tailNode (* unreachable for well-formed trees *))
        else newPath (level - BITS, tailNode)
    in
      if subidx < n then Interior (Vector.update (arr, subidx, child))
      else Interior (vAppend (arr, child))
    end

  fun push v x =
    let
      val {cnt, shift, root, tail} = unwrap v
    in
      if Vector.length tail < WIDTH then
        Vec { cnt = cnt + 1, shift = shift, root = root, tail = vAppend (tail, x) }
      else
        let
          val tailNode = Values tail
          val overflow = Word.> (Word.>> (Word.fromInt cnt, BITS), Word.<< (0w1, shift))
          val (newRoot, newShift) =
            if overflow then
              (Interior (Vector.fromList [root, newPath (shift, tailNode)]), shift + BITS)
            else
              (case root of
                   Interior arr => (pushTail (cnt, shift, arr, tailNode), shift)
                 | Values _ => (pushTail (cnt, shift, Vector.fromList [root], tailNode), shift))
        in
          Vec { cnt = cnt + 1, shift = newShift, root = newRoot, tail = Vector.fromList [x] }
        end
    end

  fun update v i x =
    let
      val {cnt, shift, root, tail} = unwrap v
    in
      if i < 0 orelse i >= cnt then raise Subscript
      else if i >= tailoff cnt then
        Vec { cnt = cnt, shift = shift, root = root,
              tail = Vector.update (tail, i - tailoff cnt, x) }
      else
        let
          fun go (Interior arr, level) =
                let val si = idx (i, level) in
                  Interior (Vector.update (arr, si, go (Vector.sub (arr, si), level - BITS)))
                end
            | go (Values vals, _) =
                Values (Vector.update (vals, Word.toInt (Word.andb (Word.fromInt i, MASK)), x))
        in
          Vec { cnt = cnt, shift = shift, root = go (root, shift), tail = tail }
        end
    end

  fun foldl f acc v =
    let
      val n = length v
      fun go (i, a) = if i >= n then a else go (i + 1, f (sub v i, a))
    in go (0, acc) end

  fun toList v =
    let fun go (i, acc) = if i < 0 then acc else go (i - 1, sub v i :: acc)
    in go (length v - 1, []) end

  fun fromList xs = List.foldl (fn (x, v) => push v x) empty xs
end
