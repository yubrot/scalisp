package scalisp

import scala.collection.mutable.{ListBuffer, StringBuilder}

class CodePrinter {
  private var id = 0
  private var blocks = new ListBuffer[StringBuilder]

  def print(): String = {
    val b = new StringBuilder
    for (block <- blocks) b ++= block
    return b.result
  }

  def putBlock(header: String, code: Code): String = {
    val label = s"[$id $header]"
    val block = new StringBuilder
    id += 1
    blocks += block
    block ++= label ++= "\n"
    for (inst <- code) block ++= "  " ++= putInst(inst) ++= "\n"
    return label
  }

  def putInst(inst: Inst): String = inst match {
    case Ldc(constant) => "ldc " + constant.inspect
    case Ldv(variable) => s"ldv $variable"
    case Ldf(pattern, code) =>
      val label = putBlock(s"fun $pattern", code)
      s"ldf $label"
    case Ldm(pattern, code) =>
      val label = putBlock(s"macro $pattern", code)
      s"ldm $label"
    case Ldb(name) => s"ldb $name"
    case Sel(a, b) =>
      val al = putBlock("then", a)
      val bl = putBlock("else", b)
      s"sel $al $bl"
    case App(argc) => s"app $argc"
    case Leave => "leave"
    case Pop => "pop"
    case Def(variable) => s"def $variable"
    case Set(variable) => s"set $variable"
  }
}

object Code {
  def unapply(code: Code): Option[String] = {
    val printer = new CodePrinter
    printer.putBlock("entry", code)
    return Some(printer.print())
  }
}
