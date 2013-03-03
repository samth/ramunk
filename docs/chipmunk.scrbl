#lang scribble/manual

@title{Chipmunk Physics}

@author{@(author+email "Jay McCarthy" "jay@plt-scheme.org")}
@author{@(author+email "Freezerburn" "Freezerburn@github.com")}
@author{@(author+email "Sam Vervaeck" "vervaeck.sam@skynet.be")}

An FFI to the @link["http://chipmunk-physics.net/"]{Chipmunk Game Dynamics} C-library.

Please refer to the @link["http://chipmunk-physics.net/documentation.php"]{official documentation} for a complete overview of all procedures.

@section{Introduction}

The library was originally written by @link["http://planet.plt-scheme.org/display.ss?package=chipmunk.plt&owner=jaymccarthy"]{Jay McCarthy}
and later updated by @link["https://github.com/Freezerburn/Rhipmunk-Physics"]{Freezerburn} and @link["https://github.com/samvv/racket-chipmunk"]{Sam Vervaeck}.

Chipmunk Physics is an FFI to the @link["http://chipmunk-physics.net/"]{Chipmunk physics engine}, which means that it uses external binaries that interact with racket.
For this reason, some platforms might not be supported. You will need to recompile the binary for your specific platform or file a request in one of the repositories.

One of the main advantages of this library is its speed. You can try out some of the demos to verify this for yourself.
For this reason this library is extremely well-suited for game applications and similar programs that require smooth physical interactions.

@section{FFI Bindings}

This section documents all of the procedures that are available to the end-user.

@defmodule["chipmunk-ffi.rkt"]

This module contains all of the bindings to the Chipmunk binaries. There is no need to import additional files.

@subsection{Vectors}

@defstruct[cpVect
           ([x cpFloat?]
           [y cpFloat?])
                        	#:extra-constructor-name cpv]

The representation of a two-dimensional vector in Chipmunk.

@defproc[(cpvadd [v1 cpVect?] [v2 cpVect?])
         cpVect?]

Produce a new @racket[cpVect] that is the sum of @racket[v1] and @racket[v2].

@defproc[(cpvsub [v1 cpVect?] [v2 cpVect?])
         cpVect?]

Produce a new @racket[cpVect] by subtracting @racket[v1] from @racket[v2].

@defproc[(cpvneg [v cpVect?])
         cpVect?]

Take the additive inverse of @racket[v] and return it as a new @racket[cpVect].

@subsection{Shapes}

A shape is used by the Chipmunk library to determine collisions between bodys.
Shapes that are added to a @racket[cpSpace] will be able to collide with each other.

@subsection{Bodys}

@subsection{Spaces}

@defstruct[cpSpace
           ([iterations _int]
            [gravity _cpVect]
            [damping _cpFloat]
            [idleSpeedThreshold _cpFloat]
            [sleepTimeThreshold _cpFloat]
            [collisionSlop _cpFloat]
            [collisionBias _cpFloat]
            [collisionPersistence _cpFloat]
            [enableContactGraph _cpBool]
            [data _cpDataPointer]
            [staticBody _cpBody-pointer])]

@section{Demos}

The @url{demos} folder contains some fully working programs to showcase the power of this library.

@section{Examples}

The @url{examples} folder contains some files that show how to use the Chipmunk API.