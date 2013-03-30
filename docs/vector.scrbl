#lang scribble/manual

@(require (for-label racket/base))

@title{Vectors}

Vectors in this library are ordinary racket vectors containing only inexact numerical values.

@defproc[(vector [c inexact?] ...)
		 vector?]{
		
 Produces an ordinary racket vector. The elements of the vector have to be @racket[inexact?] numbers.
                                                                            
}

@defproc[(vector-add [v vector?] ...)
         vector?]{

 Produce a new @racket[vector] that is the sum of all @racket[v]'s.

}
                 
@defproc[(vector-sub [v vector?] ...)
         vector?]{
                  
 Produce a new @racket[vector] by subtracting all @racket[v]'s.

}

@defproc[(vector-add! [v vector?] ...)
		 vector?]{
 Like @racket[vector-add], but mutates the first vector instead of constructing a new one.		 
}

@defproc[(vector-sub! [v vector?] ...)
		 vector?]{
 Like @racket[vector-sub], but mutates the first vector instead of constructing a new one.
}