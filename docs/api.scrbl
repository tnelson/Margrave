#lang scribble/manual

@; (Still getting warning message... why?)
@(require (for-label "../margrave.rkt"))

@title{Margrave's Racket API}



<< Racket interfaces with Java; here are the functions... >>



@section{Starting and Stopping the Java Engine}

@defproc[(start-margrave-engine 
          (margrave-home path? (current-directory)) 
          (engine-args (listof string?) empty) 
          (margrave-args (listof string?) empty))
         boolean?]{
If the Java engine is already started, returns #f.

Otherwise, starts the Java engine for a Margrave installation at
@italic{margrave-home}. The JVM is given each string in @italic{engine-args} as
arguments, and the Margrave engine is given each string in 
@italic{margrave-args} as arguments. Returns #t.
                              
}

@defproc[(stop-margrave-engine) boolean?]{                                          
If the Java engine is started, terminates it and returns #t.

Otherwise, returns #f.
}
                  
@section{Converting Results}

