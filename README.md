# star

This is an implementation of a revised definition of star.

It is sufficiently different that there may be a new name coming...

Most of the features are carried over from old star but the syntax has been
completely revised.

This repository contains the VM, two compilers -- one written in Prolog and one
written in Star --  an emacs mode to support Star, and documentation.

* The Star-in-Prolog compiler is a bootstrap compiler that generates star code
  but is not complete and will not be maintained -- except for necessary changes
  to support the main compiler. Although currently the default compiler, this
  will be changed at some point.

* The Star-in-Star compiler is the main compiler.

The documentation takes the form of three 'documents': a guide to Star that is
short and is intended to help you decide if Star is for you, an introductory
book on the language to help you learn it and a reference manual that details
the language and the standard functions and types.

The VM is effectively an interpreter for the Star byte code. The roadmap for
this includes a WebAssembly backend to the main compiler and better support for
debugging.



