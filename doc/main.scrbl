#lang scribble/manual

@title{Racket Physics Library}

@author{@(author+email "Jay McCarthy" "jay@plt-scheme.org")}
@author{@(author+email "Freezerburn" "Freezerburn@github.com")}
@author{@(author+email "Sam Vervaeck" "vervaeck.sam@skynet.be")}

An experimental physics engine for the Racket language.

@section{Introduction}

This library provides a 2D-physics engine for the Racket language. While this is still
 a work in progress, you can already try out some of the demos to get a feeling of what is availiable.
 Currently only basic physical interations are supported, such as shape-to-shape collisions and simple constraints.

The library uses the @link["http://chipmunk-physics.net/"]{Chipmunk Game Dynamics} C-library internally,
 which means that some external binaries are required for this library to work. Some platforms might not be
 supported. If you would like to add support for a certain platform you are invited to update the source code
 and merge it with the @link["https://github.com/samvv/racket-physics"]{main repository}.

@include-section["api.scrbl"]
@include-section["chipmunk.scrbl"]

@section{Examples}

The @url{examples} folder contains some files that show how to use the various classes in this library.

@section{Credits}

The @link["http://chipmunk-physics.net"]{Chipunk Physics Library} is maintained by @link["http://howlingmoonsoftware.com"]{Howling Moon Software}
 and published under the MIT License.

The FFI bindings to the Chipmunk Physics engine in Racket were originally written by @link["http://planet.plt-scheme.org/display.ss?package=chipmunk.plt&owner=jaymccarthy"]{Jay McCarthy}
 and later updated by @link["https://github.com/Freezerburn/Rhipmunk-Physics"]{Freezerburn} and @link["https://github.com/samvv/racket-chipmunk"]{Sam Vervaeck}.
