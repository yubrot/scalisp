package scalisp

import fastparse.core.Parsed._

abstract sealed class Command

class TestFailed(msg: String) extends Exception(msg)

case class ParseSuccess(input: String, result: String) extends Command
case class ParseFailure(input: String) extends Command
case class CompileSuccess(input: String, result: String) extends Command
case class CompileFailure(input: String) extends Command
case class EvalSuccess(input: String, result: String) extends Command
case class EvalFailure(input: String) extends Command
case class EvalAll(input: String) extends Command

object TestRunner {
  def run(
    context: Context,
    src: Iterator[String],
    out: String => Unit = _ => {},
    err: String => Unit = Console.err.println _
  ): Unit = {
    for (header <- src) {
      out(header)
      val command = parseCommand(src)
      try runCommand(context, command) catch {
        case e: TestFailed =>
          err(s"Test failed at $header: ${e.getMessage}")
      }
    }
  }

  def parseCommand(src: Iterator[String]): Command = {
    def readLines(len: String): String = (1 to len.toInt).map(_ => src.next).mkString("\n")

    src.next.split(" ") match {
      case Array("PARSE_SUCCESS", input, result) =>
        val i = readLines(input)
        val o = readLines(result)
        ParseSuccess(i, o)
      case Array("PARSE_FAILURE", input) =>
        val i = readLines(input)
        ParseFailure(i)
      case Array("COMPILE_SUCCESS", input, result) =>
        val i = readLines(input)
        val o = readLines(result)
        CompileSuccess(i, o)
      case Array("COMPILE_FAILURE", input) =>
        val i = readLines(input)
        CompileFailure(i)
      case Array("EVAL_SUCCESS", input, result) =>
        val i = readLines(input)
        val o = readLines(result)
        EvalSuccess(i, o)
      case Array("EVAL_FAILURE", input) =>
        val i = readLines(input)
        EvalFailure(i)
      case Array("EVAL_ALL", input) =>
        val i = readLines(input)
        EvalAll(i)
      case cmd =>
        throw new Exception("Unknown test comamnd: " + cmd(0))
    }
  }

  private def parseOrFail(i: String): Value = Parser.line.parse(i) match {
    case Success(Some(expr), _) => expr
    case _ => throw new TestFailed("Parse error")
  }

  def runCommand(context: Context, cmd: Command): Unit = cmd match {
    case ParseSuccess(i, o) =>
      Parser.line.parse(i) match {
        case Success(Some(expr), _) => if (expr.inspect != o) throw new TestFailed(expr.inspect)
        case _ => throw new TestFailed(s"Parse error $i $o")
      }
    case ParseFailure(i) =>
      Parser.line.parse(i) match {
        case Success(Some(expr), _) => throw new TestFailed(expr.inspect)
        case _ => {}
      }
    case CompileSuccess(i, o) =>
      context(_.compile(parseOrFail(i))) match {
        case Right(Code(code)) => if (code != o + "\n") throw new TestFailed(code)
        case Left(e) => throw new TestFailed(e)
      }
    case CompileFailure(i) =>
      context(_.compile(parseOrFail(i))) match {
        case Right(Code(code)) => throw new TestFailed(code)
        case _ => {}
      }
    case EvalSuccess(i, o) =>
      context(_.eval(parseOrFail(i))) match {
        case Right(value) => if (value.inspect != o) throw new TestFailed(value.inspect)
        case Left(e) => throw new TestFailed(e)
      }
    case EvalFailure(i) =>
      context(_.eval(parseOrFail(i))) match {
        case Right(value) => throw new TestFailed(value.inspect)
        case _ => {}
      }
    case EvalAll(i) =>
      Parser.program.parse(i) match {
        case Success(program, _) =>
          for (line <- program) context(_.eval(line)) match {
            case Left(e) => throw new TestFailed(e)
            case _ => {}
          }
        case _ => throw new TestFailed("Parse error")
      }
  }
}
