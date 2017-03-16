scalisp
=======

#### [Try it on browser](https://yubrot.github.io/scalisp/)

scalisp is a Scala version of [ocalisp](https://github.com/yubrot/ocalisp), a tiny Lisp-1 implementation.

    # on JVM
    $ sbt scalispJVM/assembly
    $ java -jar scalisp/jvm/target/scala-2.12/scalisp.jar lispboot/examples/conways-gol.lisp

    # on Browser
    $ git worktree add -b gh-pages gh-pages origin/gh-pages
    $ sbt scalispJS/fastOptJS

