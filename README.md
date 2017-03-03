Racket Physics Library
======================

[![Build Status](https://travis-ci.org/samvv/ramunk.svg?branch=master)](https://travis-ci.org/samvv/ramunk)

Bindings to the [Chipmunk physics engine](https://chipmunk-physics.org) for the
[Racket](https://racket-lang.org) language.

## Under the hood 

**ramunk** depends on [AutoFFI](http://github.com/samvv/racket-autoffi) for generating its bindings.
The process is fully automated, enabling rapid updates of this library to newer versions of Chipmunk.

## Unsupported Platforms

We currently only support macOS Sierra due to the young nature of the
[AutoFFI](https://github.com/AutoFFI/AutoFFI) that is used under the hood. If
you want to see your platform supported, you can help out by solving [one of
the issues](https://github.com/AutoFFI/AutoFFI/issues) regarding cross-platform
builds.

For UNIX-like systems, a simple `make binary` in the source directory should
provide you with a working binary on most platforms. It creates a new
build-directory, runs _cmake_ and _make_ in it, and copies the resulting binary
to our bin-directory.

## Credits

The FFI bindings to the Chipmunk Physics engine in Racket were originally created by [Jay McCarthy](http://planet.plt-scheme.org/display.ss?owner=jaymccarthy) and updated by [Freezerburn](https://github.com/Freezerburn).

