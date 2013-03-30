#lang scribble/manual

@title{Chipmunk Library}

This section documents the procedures that are directly available using the Chipmunk game engine.
 While the procedures in this section might be faster than their object-oriented counterparts when correctly used,
 it is recommended to use the default API for better language integration and stability.

There are two flavours of of bindings availiable: the safe bindings have a racket-like naming convention while
 the unsafe binings have C-style naming conventions. It is highly recommended not to use the unsafe bindings
 unless you have a really good reason to do so. Incorrect usage of some of those procedures can easily lead
 to memory corruption and program crashes.


Please refer to the @link["http://chipmunk-physics.net/documentation.php"]{official documentation} for a
complete overview of all procedures. Some other usefull links:
 
@itemize{
 
 @item{@link["http://chipmunk-physics.net/release/ChipmunkLatest-API-Reference/index.html"]{Chipmunk API Reference} --- The official API reference}

 @item{@link["http://chipmunk-physics.net/release/ChipmunkLatest-Docs/"]{Chipmunk Game Dynamics Manual} --- The official manual}

}

@section{Safe bindings}

@defmodule["chipmunk.rkt"]

This module contains "safe" bindings to the procedures as defined in the Chipmunk binaries.


@section{Unsafe bindings}

@defmodule["chipmunk-ffi.rkt"]

This module contains all of direct bindings to the Chipmunk binaries.
Require this file if you want low-level access to the chipmunk FFI bindings.
