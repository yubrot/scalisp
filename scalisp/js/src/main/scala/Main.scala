package scalisp

import fastparse.core.Parsed._
import scala.scalajs.js
import js.annotation._
import js.JSConverters._

@JSExportTopLevel("scalisp")
object Scalisp {
  @JSExport
  var stdout: js.Function1[js.Array[Byte], Unit] = bytes => Console.print(Str.decode(bytes.toArray))

  @JSExport
  var stderr: js.Function1[js.Array[Byte], Unit] = bytes => Console.err.print(Str.decode(bytes.toArray))

  def out(bytes: Array[Byte]): Unit = stdout(bytes.toJSArray)

  def err(bytes: Array[Byte]): Unit = stderr(bytes.toJSArray)

  private def tryEval[A](me: Either[String, A]): A = me match {
    case Right(a) => a
    case Left(e) => throw new js.JavaScriptException(e)
  }

  type Program = js.Array[Value]

  @JSExport
  def createContext(): Context = new Context

  @JSExport
  def initContext(context: Context, bootProgram: Program): Unit = {
    JSBuiltins.register(context)
    if (bootProgram != null) exec(context, bootProgram)
  }

  @JSExport
  def parse(code: String): Program = {
    Parser.program.parse(code) match {
      case Success(program, _) => (program: Seq[Value]).toJSArray
      case _ => throw new js.JavaScriptException("Parse error")
    }
  }

  @JSExport
  def exec(context: Context, program: Program): Unit = {
    for (line <- program) tryEval {
      context(_.eval(line))
    }
  }

  @JSExport
  def macroExpand(context: Context, recurse: Boolean, value: Value): Value = tryEval {
    context(_.macroExpand(recurse, value))
  }

  @JSExport
  def compile(context: Context, value: Value): Code = tryEval {
    context(_.compile(value))
  }

  @JSExport
  def runTests(test: String): Unit = {
    val context = new Context
    JSBuiltins.register(context)
    TestRunner.run(
      context,
      test.lines,
      s => out(Str.encode(s + "\n")),
      s => err(Str.encode(s + "\n")))
  }

  @JSExport
  def inspectValue(value: Value): String = value.inspect

  @JSExport
  def printCode(code: Code): String = CodePrinter.print(code)
}
