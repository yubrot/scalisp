package scalisp.cli

import scalisp.core.{Context, Builtins, Parser, TestRunner}
import scala.io.{Source, StdIn}
import scala.util.boundary

object Main:
  def main(args: Array[String]): Unit =
    val context = Context()
    args.toSeq match
      case Seq() =>
        init(context, true, Seq())
        repl(context)
      case "-test" +: tests =>
        init(context, false, Seq())
        for test <- tests do TestRunner.run(context, Source.fromFile(test).getLines)
      case ls =>
        val (files, args) = ls.span(_ != "--")
        init(context, true, if args.isEmpty then args else args.tail)
        for file <- files do execFile(context, file)

  def init(context: Context, boot: Boolean, args: Seq[String]): Unit =
    Builtins.register(context, args)
    if boot then
      exec(context, Source.fromResource("boot.lisp").mkString) match
        case Right(_) => {}
        case Left(e)  => throw RuntimeException(s"init: $e")

  def execFile(context: Context, filename: String): Unit =
    exec(context, Source.fromFile(filename).mkString) match
      case Right(_) => {}
      case Left(e)  => throw RuntimeException(s"$filename: $e")

  def exec(context: Context, code: String): Either[String, Unit] =
    Parser.program.parse(code) match
      case Right(_, program) =>
        boundary:
          for line <- program do
            context { _.eval(line) } match
              case Right(_) => {}
              case Left(e)  => boundary.break(Left(e))
      case _ => Left("Parse error")
    Right(())

  def repl(context: Context): Unit =
    Console.err.println("[scalisp REPL]")
    var buf = ""

    while true do
      Option(StdIn.readLine("> ")) match
        case Some(line) if line.endsWith("\\") =>
          buf += "\n" + line.init
        case Some(line) =>
          val input = buf + "\n" + line
          buf = ""
          Parser.program.parse(input) match
            case Right(_, exprs) =>
              for expr <- exprs do
                context { _.eval(expr) } match
                  case Right(result) => Console.println(result.inspect)
                  case Left(e)       => Console.err.println(e)
            case Left(e) =>
              Console.err.println(s"Parse error: $e")
        case None => return
