package scalisp

import scala.io._
import fastparse.core.Parsed._

object Main {
  def main(args: Array[String]): Unit = {
    val context = new Context

    args.toSeq match {
      case Seq() =>
        Console.err.println("REPL on scalisp native is not supported yet :(")
      case "-test" +: tests =>
        init(context, false, Seq())
        for (test <- tests) TestRunner.run(context, Source.fromFile(test).getLines)
      case ls =>
        val (files, args) = ls.span(_ != "--")
        init(context, true, if (args.isEmpty) args else args.tail)
        for (file <- files) execFile(context, file)
    }
  }

  def init(context: Context, boot: Boolean, args: Seq[String]): Unit = {
    CoreBuiltins.register(context, args)
    IOBuiltins.register(context)
    if (boot) {
      exec(context, Source.fromFile("lispboot/boot.lisp").mkString) match {
        case Right(_) => {}
        case Left(e) => throw new RuntimeException(s"init: $e")
      }
    }
  }

  def execFile(context: Context, filename: String): Unit = {
    exec(context, Source.fromFile(filename).mkString) match {
      case Right(_) => {}
      case Left(e) => throw new RuntimeException(s"$filename: $e")
    }
  }

  def exec(context: Context, code: String): Either[String, Unit] = {
    Parser.program.parse(code) match {
      case Success(program, _) =>
        for (line <- program) context(_.eval(line)) match {
          case Right(_) => {}
          case Left(e) => return Left(e)
        }
      case _ => Left("Parse error")
    }
    Right(())
  }
}
