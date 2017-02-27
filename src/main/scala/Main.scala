package scalisp

import scala.io._
import fastparse.core.Parsed._

object Main {
  def main(args: Array[String]): Unit = {
    val context = new Context
    boot(context)

    if (args.size == 1) {
      execFile(context, args(0))
    } else {
      repl(context)
    }
  }

  def boot(context: Context): Unit = {
    Builtins.register(context)
    exec(context, Source.fromResource("boot.lisp").mkString) match {
      case Right(_) => {}
      case Left(e) => throw new RuntimeException(e)
    }
  }

  def execFile(context: Context, filename: String): Unit = {
    exec(context, Source.fromFile(filename).mkString) match {
      case Right(_) => {}
      case Left(e) => Console.err.println(e)
    }
  }

  def exec(context: Context, code: String): Either[String, Unit] = {
    Parser.program.parse(code) match {
      case Success(program, _) =>
        for (s <- program) context.eval(s) match {
          case Right(_) => {}
          case Left(e) => return Left(e)
        }
      case _ => Left("Parse error")
    }
    Right(())
  }

  def repl(context: Context): Unit = {
    Console.err.println("[scalisp REPL]")
    val in =
      Iterator.continually(StdIn.readLine("> "))
      .takeWhile(_ != null)
      .flatMap(line => (line + "\n").split("")) // To accept strings such as "(+ 1 2) (* 3 4)"

    while (true) Parser.line.parseIterator(in) match {
      case Success(None, _) => return
      case Success(Some(expr), _) => context.eval(expr) match {
        case Right(result) => Console.println(result.inspect)
        case Left(e) => Console.err.println(e)
      }
      case _ => Console.err.println("Parse error")
    }
  }
}
