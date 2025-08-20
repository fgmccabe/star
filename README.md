# star

This is an implementation of a revised definition of star.

It is sufficiently different that there may be a new name coming...

Most of the features are carried over from old star but the syntax has been
completely revised.

This repository contains the VM, two compilers -- one written in Prolog and one
written in Star --  an emacs mode to support Star, and documentation.

* The Star-in-Prolog compiler is a bootstrap compiler that generates star code
  but is not complete and will not be maintained -- except for necessary changes
  to support the main compiler.

* The Star-in-Star compiler is the main compiler. It is written in Star.

The documentation takes the form of three 'documents': a guide to Star that is
short and is intended to help you decide if Star is for you, an introductory
book on the language to help you learn it and a reference manual that details
the language and the standard functions and types.

The VM has two modes: an interpreter for the Star byte code and jit compilation (technically, 
compile on load). The JIT compiler is pretty 'rough'; it is not intended to be a super
optimizing compiler. The byte code interpreter is there for scenarios where jitting is not 
permitted or supported.

Currently, the only supported architecture is ARM64. There will be an X64 jit compiler at some
time. There is currently no intention to support 32-bit architectures.

The roadmap for the Star language includes a WebAssembly backend to the main compiler and
better support for debugging.



