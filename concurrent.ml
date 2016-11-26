open Lwt_condition

let waitingWriters = ref 0
let waitingReaders = ref 0
let nReaders = ref 0
let nWriters = ref 0
let canRead = create ()
let canWrite = create ()

let m = Lwt_mutex.create ()

let beginWrite () =
  Lwt_mutex.lock m;
  waitingWriters := !waitingWriters + 1;
  while !nWriters	> 0	|| !nReaders	> 0 do
    wait canWrite m;
    waitingWriters := !waitingWriters - 1;
  done;
  nWriters := 1;
  Lwt_mutex.unlock m;
  ()

let endWrite () =
  Lwt_mutex.lock m;
  nWriters	:=	0;
  let _ =
    if !waitingWriters	>	0 then  (broadcast canWrite;)
    else if	(!waitingReaders	>	0) then (broadcast canRead;)
    else ignore ()
  in
  Lwt_mutex.unlock m;
  ()

let beginRead () =
  Lwt_mutex.lock m;
  waitingReaders := !waitingReaders + 1;
  while	(!nWriters>0	||	!waitingWriters>0) do
    wait canRead m;
  done;
  waitingReaders := !waitingReaders - 1;
  nReaders := !nReaders + 1;
  Lwt_mutex.unlock m;
  ()

let endRead () =
  Lwt_mutex.lock m;
  nReaders := !nReaders - 1;
  if	(!nReaders = 0	&&	!waitingWriters > 0) then
    broadcast canWrite;
  Lwt_mutex.unlock m;
  ()
