open Condition

let waitingWriters = ref 0
let waitingReaders = ref 0
let nReaders = ref 0
let nWriters = ref 0
let canRead = create ()
let canWrite = create ()

let lock = Mutex.create ()



let beginWrite () =
  if !nReaders <> 0 || !nWriters <> 0 then
    failwith "Can't begin writing; nRreaders or nWriters <> 0"
  waitingWriters := !waitingWriters + 1;
  while !nWriters	> 0	|| !nReaders	> 0 do
    wait canWrite lock;
    waitingWriters := !waitingWriters - 1;
  done;
  nWriters := 1;

let endWrite () =
  if !nReaders <> 0 || !nWriters <> 0 then
    failwith "Can't begin writing; nRreaders or nWriters <> 0"
  waitingWriters := !waitingWriters + 1;
  while !nWriters	> 0	|| !nReaders	> 0 do
    wait canWrite lock;
    waitingWriters := !waitingWriters - 1;
  done;
  nWriters := 1;

let beginRead () =
  failwith "unimplemented"

let endRead () = failwith "unimplemented"



(*
int	waitingWriters=0,	waitingReaders=0,	nReaders=0,	nWriters=0;
	Condition	canRead,	canWrite;
	void	BeginWrite()
		assert(nReaders==0	or	nWriters==0)
		++waitingWriters
		while	(nWriters	>0	or	nReaders	>0)
	 		canWrite.wait();
		--waitingWriters
		nWriters	=	1;
	void	EndWrite()
		assert(nWriters==1	and	nReaders==0)
		nWriters	=	0
		if	WaitingWriters	>	0
	 		canWrite.signal();
		else	if	waitingReaders	>	0
				canRead.broadcast();
void	BeginRead()
	 assert(nReaders==0	or	nWriters==0)
	 ++waitingReaders
	 while	(nWriters>0	or	waitingWriters>0)
	 	 canRead.wait();
	 --waitingReaders
	 ++nReaders
void	EndRead()
	 assert(nReaders>0	and	nWriters==0)
	 --nReaders;
	 if	(nReaders==0	and	waitingWriters>0)
						canWrite.signal(); *)
