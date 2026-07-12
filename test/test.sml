(* Tests for sml-hamt.

   The map suite drives HamtMap against an assoc-list oracle using a
   deterministic LCG-generated sequence of insert/remove operations,
   including a deliberately weak hash to exercise collision nodes. The
   immutability suite checks that older versions are unaffected by later
   updates. The vector suite checks push/sub/update/length/order across
   several trie levels. *)

structure HamtTests =
struct
  open Harness

  (* ---------- deterministic LCG (Numerical Recipes constants) ---------- *)
  fun mkRng seed =
    let val s = ref (seed : Word32.word)
    in fn () => (s := Word32.+ (Word32.* (!s, 0w1664525), 0w1013904223); !s)
    end

  fun nextInt rng bound =
    Word32.toInt (Word32.mod (rng (), Word32.fromInt bound))

  (* ---------- list helpers (portable, no library sort) ---------- *)
  fun insSortBy cmp xs =
    let
      fun ins x [] = [x]
        | ins x (y :: ys) = if cmp (x, y) <> GREATER then x :: y :: ys
                            else y :: ins x ys
    in List.foldr (fn (x, acc) => ins x acc) [] xs end

  fun byKey ((k1, _), (k2, _)) = Int.compare (k1, k2)
  fun sortPairs xs = insSortBy byKey xs

  (* ---------- assoc-list oracle ---------- *)
  fun oFind orc k = Option.map #2 (List.find (fn (k', _) => k' = k) orc)
  fun oInsert orc k v = (k, v) :: List.filter (fn (k', _) => k' <> k) orc
  fun oRemove orc k = List.filter (fn (k', _) => k' <> k) orc

  (* ---------- one randomized run against the oracle ---------- *)
  fun oracleRun {label, hash, seed, steps, universe} =
    let
      val rng = mkRng seed
      val keys = List.tabulate (universe, fn i => i)
      val empty = HamtMap.empty {hash = hash, eq = (op =) : int * int -> bool}

      fun mapsEqual (m, orc) =
        HamtMap.size m = length orc
        andalso List.all (fn k => HamtMap.find m k = oFind orc k) keys
        andalso sortPairs (HamtMap.toList m) = sortPairs orc

      fun loop (m, orc, i, allOk) =
        if i >= steps then (m, orc, allOk)
        else
          let
            val opc = nextInt rng 3
            val k = nextInt rng universe
            val (m', orc') =
              if opc = 2 then (HamtMap.remove m k, oRemove orc k)
              else let val v = nextInt rng 1000
                   in (HamtMap.insert m k v, oInsert orc k v) end
            val ok = allOk andalso mapsEqual (m', orc')
          in loop (m', orc', i + 1, ok) end

      val (mFinal, orcFinal, allOk) = loop (empty, [], 0, true)
    in
      checkBool (label ^ ": matches oracle at every step") (true, allOk);
      checkInt (label ^ ": final size") (length orcFinal, HamtMap.size mFinal);
      checkBool (label ^ ": final toList (sorted) matches")
        (true, sortPairs (HamtMap.toList mFinal) = sortPairs orcFinal)
    end

  fun run () =
    let
      (* ---------------- Map vs oracle ---------------- *)
      val () = section "HamtMap vs assoc-list oracle"

      (* A reasonably mixing hash over the key space. *)
      fun mixHash k =
        let
          val w = Word.fromInt k
          val w = Word.xorb (w, Word.<< (w, 0w13))
          val w = Word.xorb (w, Word.>> (w, 0w7))
          val w = Word.xorb (w, Word.<< (w, 0w17))
        in w end
      (* Deliberately weak hash: only 8 distinct buckets => lots of
         collisions across a 64-key universe, exercising collision nodes. *)
      fun weakHash k = Word.fromInt (k mod 8)
      (* Pathological: every key collides into a single bucket. *)
      fun constHash _ = 0w0

      val () = oracleRun {label = "mixing hash", hash = mixHash,
                          seed = 0wx12345, steps = 3000, universe = 200}
      val () = oracleRun {label = "weak hash", hash = weakHash,
                          seed = 0wxC0FFEE, steps = 3000, universe = 64}
      val () = oracleRun {label = "single-bucket hash", hash = constHash,
                          seed = 0wxBEEF, steps = 1500, universe = 40}

      (* ---------------- Collision nodes (explicit) ---------------- *)
      val () = section "HamtMap collision handling"
      val cm0 = HamtMap.empty {hash = (fn _ => 0w0), eq = (op =) : int*int->bool}
      val cm1 = HamtMap.insert cm0 1 10
      val cm2 = HamtMap.insert cm1 2 20
      val cm3 = HamtMap.insert cm2 3 30
      val () = checkInt "three colliding keys -> size 3" (3, HamtMap.size cm3)
      val () = checkBool "find colliding key 1" (true, HamtMap.find cm3 1 = SOME 10)
      val () = checkBool "find colliding key 2" (true, HamtMap.find cm3 2 = SOME 20)
      val () = checkBool "find colliding key 3" (true, HamtMap.find cm3 3 = SOME 30)
      val cm4 = HamtMap.insert cm3 2 222
      val () = checkBool "overwrite within collision node"
                 (true, HamtMap.find cm4 2 = SOME 222)
      val () = checkInt "overwrite keeps size 3" (3, HamtMap.size cm4)
      val cm5 = HamtMap.remove cm4 2
      val () = checkBool "remove from collision node" (true, HamtMap.find cm5 2 = NONE)
      val () = checkInt "remove from collision -> size 2" (2, HamtMap.size cm5)
      val () = checkBool "remaining colliding key still present"
                 (true, HamtMap.find cm5 1 = SOME 10 andalso HamtMap.find cm5 3 = SOME 30)

      (* ---------------- Basic map ops ---------------- *)
      val () = section "HamtMap basics"
      val e = HamtMap.empty {hash = mixHash, eq = (op =) : int*int->bool}
      val () = checkBool "empty isEmpty" (true, HamtMap.isEmpty e)
      val () = checkInt "empty size" (0, HamtMap.size e)
      val () = checkBool "empty find -> NONE" (true, HamtMap.find e 42 = NONE)
      val () = checkBool "empty remove no-op" (true, HamtMap.isEmpty (HamtMap.remove e 42))
      val m1 = HamtMap.insert e 7 70
      val () = checkBool "single insert find" (true, HamtMap.find m1 7 = SOME 70)
      val () = checkBool "contains true" (true, HamtMap.contains m1 7)
      val () = checkBool "contains false" (false, HamtMap.contains m1 8)
      val () = checkInt "foldl sums values"
                 (70, HamtMap.foldl (fn (_, v, a) => a + v) 0 m1)

      (* ---------------- Immutability / structural sharing ---------------- *)
      val () = section "HamtMap immutability"
      val base =
        List.foldl (fn (k, m) => HamtMap.insert m k (k * 100)) e
          [1, 2, 3, 4, 5, 100, 101, 102, 8192, 8193]
      val snapshotSize = HamtMap.size base
      (* Inserting a brand new key must not affect `base`. *)
      val withNew = HamtMap.insert base 9999 1
      val () = checkBool "old version lacks new key" (true, HamtMap.find base 9999 = NONE)
      val () = checkBool "new version has new key" (true, HamtMap.find withNew 9999 = SOME 1)
      val () = checkInt "old version size unchanged" (snapshotSize, HamtMap.size base)
      (* Overwriting an existing key must not affect `base`. *)
      val withOver = HamtMap.insert base 3 99999
      val () = checkBool "old version keeps old value" (true, HamtMap.find base 3 = SOME 300)
      val () = checkBool "new version has new value" (true, HamtMap.find withOver 3 = SOME 99999)
      (* Removing must not affect `base`. *)
      val withRem = HamtMap.remove base 3
      val () = checkBool "old version still has removed key" (true, HamtMap.find base 3 = SOME 300)
      val () = checkBool "new version lacks removed key" (true, HamtMap.find withRem 3 = NONE)
      val () = checkInt "old size after remove on copy" (snapshotSize, HamtMap.size base)

      (* ---------------- PVec ---------------- *)
      val () = section "PVec basics"
      val () = checkBool "empty isEmpty" (true, PVec.isEmpty PVec.empty)
      val () = checkInt "empty length" (0, PVec.length (PVec.empty : int PVec.t))
      val () = checkRaises "empty sub raises" (fn () => PVec.sub (PVec.empty : int PVec.t) 0)

      val n = 5000
      val big = List.foldl (fn (i, v) => PVec.push v (i * 3)) (PVec.empty : int PVec.t)
                  (List.tabulate (n, fn i => i))
      val () = checkInt "push N -> length" (n, PVec.length big)
      val () = checkBool "all subs correct"
                 (true, List.all (fn i => PVec.sub big i = i * 3) (List.tabulate (n, fn i => i)))
      val () = checkBool "sub 0" (true, PVec.sub big 0 = 0)
      val () = checkBool "sub last" (true, PVec.sub big (n - 1) = (n - 1) * 3)
      val () = checkRaises "sub out of range raises" (fn () => PVec.sub big n)
      val () = checkRaises "sub negative raises" (fn () => PVec.sub big (~1))
      val () = checkBool "toList order preserved"
                 (true, PVec.toList big = List.tabulate (n, fn i => i * 3))
      val () = checkBool "foldl sums in order"
                 (true, PVec.foldl (fn (x, a) => a + x) 0 big
                        = List.foldl (fn (i, a) => a + i * 3) 0 (List.tabulate (n, fn i => i)))

      val () = section "PVec immutability"
      val mid = n div 2
      val upd = PVec.update big mid ~7
      val () = checkBool "updated index has new value" (true, PVec.sub upd mid = ~7)
      val () = checkBool "original index unchanged" (true, PVec.sub big mid = mid * 3)
      val () = checkInt "update preserves length" (n, PVec.length upd)
      val () = checkBool "update leaves other indices intact"
                 (true, PVec.sub upd 0 = 0 andalso PVec.sub upd (n - 1) = (n - 1) * 3)
      val () = checkRaises "update out of range raises" (fn () => PVec.update big n 0)

      val () = section "PVec fromList / roundtrip"
      val xs = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
      val v = PVec.fromList xs
      val () = checkInt "fromList length" (10, PVec.length v)
      val () = checkBool "fromList toList roundtrip" (true, PVec.toList v = xs)
      val () = checkBool "fromList then push" (true, PVec.toList (PVec.push v 42) = xs @ [42])

      (* ---------------- properties (sml-check) ---------------- *)
      val () = section "HamtMap properties (sml-check)"

      val smallInt = Check.choose (~1000, 1000)
      val genPair = Check.tuple2 (smallInt, smallInt)
      val genList = Check.listOf genPair

      fun showPair (k, v) = "(" ^ Int.toString k ^ "," ^ Int.toString v ^ ")"
      fun showPairList xs = "[" ^ String.concatWith "," (List.map showPair xs) ^ "]"
      fun showIntList xs = "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"

      fun dedupKeys ks =
        List.foldr
          (fn (k, acc) => if List.exists (fn k' => k' = k) acc then acc else k :: acc)
          [] ks

      val eqInt = (op =) : int * int -> bool
      fun freshMap () = HamtMap.empty {hash = mixHash, eq = eqInt}
      fun buildFromPairs xs =
        List.foldl (fn ((k, v), m) => HamtMap.insert m k v) (freshMap ()) xs

      (* insert-then-find returns the inserted value. *)
      val () =
        Harness.check "prop: insert-then-find returns the inserted value"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple3 (genList, smallInt, smallInt))
                     (fn (xs, k, v) => showPairList xs ^ " k=" ^ Int.toString k
                                        ^ " v=" ^ Int.toString v)
                     (fn (xs, k, v) =>
                        let val m = buildFromPairs xs
                            val m' = HamtMap.insert m k v
                        in HamtMap.find m' k = SOME v end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* remove-then-find returns NONE, whether or not the key was present. *)
      val () =
        Harness.check "prop: remove-then-find returns NONE"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple2 (genList, smallInt))
                     (fn (xs, k) => showPairList xs ^ " k=" ^ Int.toString k)
                     (fn (xs, k) =>
                        let val m = buildFromPairs xs
                            val m' = HamtMap.remove m k
                        in HamtMap.find m' k = NONE end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* toList (unordered) matches the assoc-list oracle exactly, once both
         sides are sorted -- i.e. the map contains exactly the inserted keys,
         each bound to its last-written value. *)
      val () =
        Harness.check "prop: toList matches the assoc-list oracle"
          (case Check.quickCheck
                  (Check.forAll genList showPairList
                     (fn xs =>
                        let
                          val m = buildFromPairs xs
                          val orc = List.foldl (fn ((k, v), acc) => oInsert acc k v) [] xs
                        in sortPairs (HamtMap.toList m) = sortPairs orc end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* inserting a set of distinct keys yields a map of exactly that
         size. *)
      val () =
        Harness.check "prop: size after inserting N distinct keys = N"
          (case Check.quickCheck
                  (Check.forAll (Check.listOf smallInt) showIntList
                     (fn ks =>
                        let val distinct = dedupKeys ks
                            val m = buildFromPairs (List.map (fn k => (k, k)) distinct)
                        in HamtMap.size m = List.length distinct end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* re-inserting the same key with a different value overwrites in
         place: the size doesn't grow and the new value wins. *)
      val () =
        Harness.check "prop: insert on an existing key overwrites, not duplicates"
          (case Check.quickCheck
                  (Check.forAll
                     (Check.tuple2 (genList, Check.tuple3 (smallInt, smallInt, smallInt)))
                     (fn (xs, (k, v1, v2)) =>
                        showPairList xs ^ " k=" ^ Int.toString k
                        ^ " v1=" ^ Int.toString v1 ^ " v2=" ^ Int.toString v2)
                     (fn (xs, (k, v1, v2)) =>
                        let val m = buildFromPairs xs
                            val m1 = HamtMap.insert m k v1
                            val m2 = HamtMap.insert m1 k v2
                        in HamtMap.find m2 k = SOME v2
                           andalso HamtMap.size m2 = HamtMap.size m1
                        end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)
    in
      ()
    end
end
