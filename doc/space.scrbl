#lang scribble/manual

@(require (for-label racket/base))

@title{Spaces}

@defclass[space% object% ()]{
                             
 Representation of a physical space.
                             
 @defconstructor[([iterations integer? 10]
                  [gravity vector? (vector 0.0 0.0)]
                  [damping inexact? (vector 0.0 0.0)])]{

  Create a new space with physical properties.

 }
 
 @defmethod[(get-gravity) vector?]{
                                    
 }
 
}