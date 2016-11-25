open Batteries

val waitingWriters : int
val waitingReaders : int
val nReaders : int
val nWriters : int


val beginWrite: unit -> unit

val endWrite: unit -> unit

val beginRead: unit -> unit

val endRead: unit -> unit
