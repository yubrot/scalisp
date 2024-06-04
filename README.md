scalisp
=======

scalisp is a Scala version of [Rosetta Lisp](https://github.com/yubrot/rosetta-lisp) implementation.

```bash
$ sbt "cli/run ..."

# .. or ..

$ sbt cli/assembly
$ java -jar cli/target/scala-3.3.3/scalisp.jar ...
```

There is [an implementation that runs in the browser](https://yubrot.github.io/scalisp/), but it is written in Scala 2 and uses Scala.js. The current `main` branch is written in Scala 3 and supports only JVM, so if you are interested, please check the `scala-2` branch.

