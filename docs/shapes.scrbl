#lang scribble/manual

@(require (for-label racket/base))

@title{Shapes}

@defclass[shape% object% ()]{

A shape is used to determine collisions between bodys.
Shapes that are added to a @racket[space%] will be able to collide with each other.

}

@section{Segment Shapes}

@defclass[segment-shape% shape% ()]{
 
 A primitive line shape.
 
 @defconstructor[([elasticity 0.0]
				   [body (is-a/c? body%)])]{
  
  Create a new @racket[shape%] with the given @racket[elasticity]. The elasticity determines
  the resulting velocity of a body after a collision with the shape. The @racket[body]
  is required when constructing a new shape.
  
 }
 
 @defmethod[(get-elasticity) inexact?]{
  
  Gives back the elasticity of this shape.
  
 }
 
 @defmethod[(set-elasticity [elasticity inexact?]) void?]{
 
  Change the elasticity of this shape to a new value. The value must be an inexact number
  and is usually layered between @racket[0.0] and @racket[1.0].
 
 }
 
 @defmethod[(get-body) (is-a/c? body%)]{
  
  Gives back the @racket[body%] object this shape belongs to.
  
 }
 
 @defmethod[(set-body [body (is-a/c? body%)]) (is-a/c? body%)]{
 
  Binds the shape to the designated @racket[body].
 
 }
 
}

@section{Circle Shapes}

@defclass[circle-shape% shape% ()]{

 A circle shape.
 
 @defconstructor[([radius integer?])]{
  
  Constructs a circle with the given @racket[radius].
  
 }

}

@section{Polygonic Shapes}

@defclass[poly-shape% shape% ()]{
 A complex polygonic shape.
}

@defclass[box-shape% poly-shape% ()]{
 Derivative of a polygonic shape.
}
