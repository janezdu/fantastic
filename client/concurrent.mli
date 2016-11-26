(* before code that reads the model, write beginRead () *)
val beginWrite: unit -> unit

(* after you're done reading, call endRead () *)
val endWrite: unit -> unit

(* if you're writiing (changing) model, put beginWrite ()
 * before the lines that change model *)
val beginRead: unit -> unit

(* put endWrite () ; after reading *)
val endRead: unit -> unit