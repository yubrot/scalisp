scalisp
=======

#### [Try it on browser](https://yubrot.github.io/scalisp/)

scalisp is a Scala version of [Rosetta Lisp](https://github.com/yubrot/rosetta-lisp) implementation.

    # Run on JVM
    $ mill scalisp.jvm.assembly
    $ java -jar out/scalisp/jvm/assembly/dest/out.jar rosetta-lisp/examples/conways-gol.lisp

    # Run natively
    $ mill scalisp.native.nativeLink
    $ ./out/scalisp/native/nativeLink/dest/out rosetta-lisp/examples/conways-gol.lisp

    # Run on Browser
    $ git worktree add -b gh-pages gh-pages origin/gh-pages
    $ mill scalisp.js.fastOpt

