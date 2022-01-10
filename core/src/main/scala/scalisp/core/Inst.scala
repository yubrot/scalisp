package scalisp.core

import scala.collection.mutable.{ListBuffer, StringBuilder}

enum Inst:
  case Ldc(constant: Value)
  case Ldv(variable: String)
  case Ldf(pattern: Pattern, code: Code)
  case Ldm(pattern: Pattern, code: Code)
  case Ldb(name: String)
  case Sel(a: Code, b: Code)
  case App(argc: Int)
  case Leave
  case Pop
  case Def(variable: String)
  case Set(variable: String)

type Code = Seq[Inst]

object Code:
  def print(code: Code): String =
    val printer = Printer()
    printer.block("entry", code)
    printer.print()

  class Printer:
    private var id = 0
    private var blocks = ListBuffer[StringBuilder]()

    def print(): String =
      val b = StringBuilder()
      for block <- blocks do b ++= block
      b.result

    def block(header: String, code: Code): String =
      val label = s"[$id $header]"
      val block = StringBuilder()
      id += 1
      blocks += block
      block ++= label ++= "\n"
      for i <- code do block ++= "  " ++= inst(i) ++= "\n"
      label

    def inst(inst: Inst): String = inst match
      case Inst.Ldc(constant) => "ldc " + constant.inspect
      case Inst.Ldv(variable) => s"ldv $variable"
      case Inst.Ldf(pattern, code) =>
        val label = block(s"fun $pattern", code)
        s"ldf $label"
      case Inst.Ldm(pattern, code) =>
        val label = block(s"macro $pattern", code)
        s"ldm $label"
      case Inst.Ldb(name) => s"ldb $name"
      case Inst.Sel(a, b) =>
        val al = block("then", a)
        val bl = block("else", b)
        s"sel $al $bl"
      case Inst.App(argc)     => s"app $argc"
      case Inst.Leave         => "leave"
      case Inst.Pop           => "pop"
      case Inst.Def(variable) => s"def $variable"
      case Inst.Set(variable) => s"set $variable"
