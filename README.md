Racket Physics Library
======================

An experimental physics engine for the Racket language.

## Under the hood 

**ramunk** depends on [AutoFFI](http://github.com/samvv/AutoFFI) for generating its bindings.

## Unsupported platforms

We provide working copies of the chipmunk phyics library for a
variety of platforms. However, not all platforms are supported
out-of-the-box, so you might need to
[open an issue](http://github.com/samvv/ramunk/issues/new).

If you need a binary right now, you can use the following instructions.

### On UNIX-like platforms

A simple `make`-command in the source directory should provide you
with a working binary on most platforms. It creates a new
build-directory, runs _cmake_ and _make_ in it, and copies the
resulting binary to our bin-directory.

### On Windows

This is a work in progress.

## Credits

The FFI bindings to the Chipmunk Physics engine in Racket were originally created by [Jay McCarthy](http://planet.plt-scheme.org/display.ss?owner=jaymccarthy) and updated by [Freezerburn](https://github.com/Freezerburn).

