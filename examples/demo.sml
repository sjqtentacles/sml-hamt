(* demo.sml -- build a HamtMap and a PVec, print lookups, and show that
   older versions are untouched by later updates (structural sharing). *)

(* A simple deterministic string hash (FNV-1a, 32-bit). *)
fun hashString s =
  let
    val prime = 0w16777619
    fun step (c, h) = Word.* (Word.xorb (h, Word.fromInt (Char.ord c)), prime)
  in
    CharVector.foldl step 0wx811c9dc5 s
  end

fun showOpt NONE = "<none>"
  | showOpt (SOME v) = Int.toString v

val () = print "== HamtMap ==\n"

val m0 = HamtMap.empty {hash = hashString, eq = (op =) : string * string -> bool}
val m1 = HamtMap.insert m0 "apple" 1
val m2 = HamtMap.insert m1 "banana" 2
val m3 = HamtMap.insert m2 "cherry" 3
(* m4 overwrites "banana"; m2 must keep the old value. *)
val m4 = HamtMap.insert m3 "banana" 200

val () = print ("size m4         = " ^ Int.toString (HamtMap.size m4) ^ "\n")
val () = print ("find m4 cherry  = " ^ showOpt (HamtMap.find m4 "cherry") ^ "\n")
val () = print ("find m4 banana  = " ^ showOpt (HamtMap.find m4 "banana") ^ "\n")
val () = print ("find m3 banana  = " ^ showOpt (HamtMap.find m3 "banana")
                ^ "  (older version unchanged)\n")
val () = print ("find m4 durian  = " ^ showOpt (HamtMap.find m4 "durian") ^ "\n")

val m5 = HamtMap.remove m4 "apple"
val () = print ("after remove apple: size m5 = " ^ Int.toString (HamtMap.size m5)
                ^ ", size m4 = " ^ Int.toString (HamtMap.size m4)
                ^ "  (m4 unchanged)\n")

val () = print "\n== PVec ==\n"

val v0 = PVec.fromList [10, 20, 30, 40, 50]
val v1 = PVec.push v0 60
(* v2 updates index 2; v0/v1 keep the original value. *)
val v2 = PVec.update v1 2 999

fun showVec v = "[" ^ String.concatWith "," (List.map Int.toString (PVec.toList v)) ^ "]"

val () = print ("v0          = " ^ showVec v0 ^ "\n")
val () = print ("v1 = push 60= " ^ showVec v1 ^ "\n")
val () = print ("v2 = upd@2  = " ^ showVec v2 ^ "\n")
val () = print ("v1 still    = " ^ showVec v1 ^ "  (update did not mutate v1)\n")
val () = print ("sub v2 2    = " ^ Int.toString (PVec.sub v2 2) ^ "\n")
val () = print ("length v2   = " ^ Int.toString (PVec.length v2) ^ "\n")
