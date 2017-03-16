package scalisp

import fastparse.core.Parsed._
import scala.scalajs.js
import js.annotation.JSExport
import js.JSConverters._

class Interrupt extends Exception

@JSExport
object Main {
  private def failure(e: String): Nothing = {
    Console.err.println(e)
    throw new Interrupt
  }

  private def mayFailure[A](me: Either[String, A]): A = me match {
    case Left(e) => failure(e)
    case Right(a) => a
  }

  @JSExport
  def printValue(value: Value): Unit = println(value.inspect)

  @JSExport
  def printCode(code: Code): Unit = println(CodePrinter.print(code))

  @JSExport
  def createContext(boot: Program): Context = {
    val context = new Context
    Builtins.register(context)
    exec(context, boot)
    context
  }

  type Program = js.Array[Value]

  @JSExport
  def parse(code: String): Program = {
    Parser.program.parse(code) match {
      case Success(program, _) => (program: Seq[Value]).toJSArray
      case _ => failure("Parse error")
    }
  }

  @JSExport
  def macroExpand(context: Context, recurse: Boolean, value: Value): Value = mayFailure(context(_.macroExpand(recurse, value)))

  @JSExport
  def compile(context: Context, value: Value): Code = mayFailure(context(_.compile(value)))

  @JSExport
  def exec(context: Context, program: Program): Unit = for (line <- program) mayFailure(context(_.eval(line)))

  @JSExport
  def runTests(test: String): Unit = {
    val context = new Context
    Builtins.register(context)
    TestRunner.run(context, test.lines, true)
  }
}
