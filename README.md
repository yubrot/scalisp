scalisp
=======

#### [Try it on browser](https://yubrot.github.io/scalisp/)

scalisp is a Scala version of [ocalisp](https://github.com/yubrot/ocalisp), a tiny Lisp-1 implementation.

    # Run on JVM
    $ mill scalisp.jvm.assembly
    $ java -jar out/scalisp/jvm/assembly/dest/out.jar lispboot/examples/conways-gol.lisp

    # Run natively
    $ mill scalisp.native.nativeLink
    $ ./out/scalisp/native/nativeLink/dest/out lispboot/examples/conways-gol.lisp

    # Run on Browser
    $ git worktree add -b gh-pages gh-pages origin/gh-pages
    $ mill scalisp.js.fastOpt

