package scalisp.core

class TestFailed(msg: String) extends Exception(msg)

enum Command:
  case ParseSuccess(input: String, result: String)
  case ParseFailure(input: String)
  case CompileSuccess(input: String, result: String)
  case CompileFailure(input: String)
  case EvalSuccess(input: String, result: String)
  case EvalFailure(input: String)
  case EvalAll(input: String)

object TestRunner:
  def run(
      context: Context,
      src: Iterator[String],
      out: String => Unit = _ => {},
      err: String => Unit = Console.err.println _
  ): Unit =
    for header <- src do
      out(header)
      val command = parseCommand(src)
      try runCommand(context, command)
      catch case e: TestFailed => err(s"Test failed at $header: ${e.getMessage}")

  def parseCommand(src: Iterator[String]): Command =
    def readLines(len: String): String = (1 to len.toInt).map(_ => src.next).mkString("\n")

    src.next.split(" ") match
      case Array("PARSE_SUCCESS", input, result) =>
        val i = readLines(input)
        val o = readLines(result)
        Command.ParseSuccess(i, o)
      case Array("PARSE_FAILURE", input) =>
        val i = readLines(input)
        Command.ParseFailure(i)
      case Array("COMPILE_SUCCESS", input, result) =>
        val i = readLines(input)
        val o = readLines(result)
        Command.CompileSuccess(i, o)
      case Array("COMPILE_FAILURE", input) =>
        val i = readLines(input)
        Command.CompileFailure(i)
      case Array("EVAL_SUCCESS", input, result) =>
        val i = readLines(input)
        val o = readLines(result)
        Command.EvalSuccess(i, o)
      case Array("EVAL_FAILURE", input) =>
        val i = readLines(input)
        Command.EvalFailure(i)
      case Array("EVAL_ALL", input) =>
        val i = readLines(input)
        Command.EvalAll(i)
      case cmd =>
        throw Exception("Unknown test comamnd: " + cmd(0))

  private def parseOrFail(i: String): Value =
    Parser.unit.parse(i) match
      case Right(_, Some(expr)) => expr
      case _                    => throw TestFailed("Parse error")

  def runCommand(context: Context, cmd: Command): Unit =
    cmd match
      case Command.ParseSuccess(i, o) =>
        Parser.unit.parse(i) match
          case Right(_, Some(expr)) => if expr.inspect != o then throw TestFailed(expr.inspect)
          case Right(_, None)       => throw TestFailed("Parse error: eof")
          case Left(e)              => throw TestFailed(s"Parse error: $e")
      case Command.ParseFailure(i) =>
        Parser.unit.parse(i) match
          case Right(_, Some(expr)) => throw TestFailed(expr.inspect)
          case _                    => {}
      case Command.CompileSuccess(i, o) =>
        context { _.compile(parseOrFail(i)) } match
          case Right(code) => if Code.print(code) != o + "\n" then throw TestFailed(Code.print(code))
          case Left(e)     => throw TestFailed(e)
      case Command.CompileFailure(i) =>
        context { _.compile(parseOrFail(i)) } match
          case Right(code) => throw TestFailed(Code.print(code))
          case _           => {}
      case Command.EvalSuccess(i, o) =>
        context { _.eval(parseOrFail(i)) } match
          case Right(value) => if value.inspect != o then throw TestFailed(value.inspect)
          case Left(e)      => throw TestFailed(e)
      case Command.EvalFailure(i) =>
        context { _.eval(parseOrFail(i)) } match
          case Right(value) => throw TestFailed(value.inspect)
          case _            => {}
      case Command.EvalAll(i) =>
        Parser.program.parse(i) match
          case Right(_, program) =>
            for line <- program do
              context { _.eval(line) } match
                case Left(e) => throw TestFailed(e)
                case _       => {}
          case _ => throw TestFailed("Parse error")
