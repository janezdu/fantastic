open Condition

let waitingWriters = ref 0
let waitingReaders = ref 0
let nReaders = ref 0
let nWriters = ref 0
let canRead = create ()
let canWrite = create ()

let m = Mutex.create ()

let beginWrite () =
  Mutex.lock m;
  waitingWriters := !waitingWriters + 1;
  while !nWriters	> 0	|| !nReaders	> 0 do
    wait canWrite m;
    waitingWriters := !waitingWriters - 1;
  done;
  nWriters := 1;
  Mutex.unlock m;
  ()

let endWrite () =
  Mutex.lock m;
  nWriters	:=	0;
  let _ =
    if !waitingWriters	>	0 then  (broadcast canWrite;)
    else if	(!waitingReaders	>	0) then (broadcast canRead;)
    else ignore ()
  in
  Mutex.unlock m;
  ()

let beginRead () =
  Mutex.lock m;
  waitingReaders := !waitingReaders + 1;
  while	(!nWriters>0	||	!waitingWriters>0) do
    wait canRead m;
  done;
  waitingReaders := !waitingReaders - 1;
  nReaders := !nReaders + 1;
  Mutex.unlock m;
  ()

let endRead () =
  Mutex.lock m;
  nReaders := !nReaders - 1;
  if	(!nReaders = 0	&&	!waitingWriters > 0) then
    broadcast canWrite;
  Mutex.unlock m;
  ()
