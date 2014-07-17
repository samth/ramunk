#lang scribble/manual

@(require (for-label racket/base))

@title{Bodys}

@defclass[body% object% ()]{

 Represenation of a physical body.
 
 @defconstructor[([mass inexact? #f]
				  [moment inexact? #f]
				  [position vector? (vector 0.0 0.0)]
				  [velocity vector? (vector 0.0 0.0)])]{
	 
  Constructs a new body. When no mass and moment are given, the body is considered
  to be a static body.

 }
 
 @defmethod[(get-position) vector?]{
	
	Gives back the position of this body in absolute world coordinates.
	
 }
 
 @defmethod[(get-velocity) vector?]{

  Gives back the velocity of this body as a vector.
 
 }
 
 @defmethod[(get-angle) inexact?]{
 
  Gives back the angle of this body in radians.
 
 }
 
 @defmethod[(get-angular-velocity) inexact?]{
 
  Gives back the angular velocity of this body in radians.
 
 }

}