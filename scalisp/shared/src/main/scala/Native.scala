package scalisp

abstract sealed class Native extends Inspect {
  def isProc(): Boolean = false
  def isMeta(): Boolean = false
  def isPort(): Boolean = false
  def isVec(): Boolean = false
}

private[scalisp]
case class Fun(env: Env[Value], pattern: Pattern, code: Code) extends Native {
  override def isProc(): Boolean = true
  def inspect(): String = "<fun>"
}

private[scalisp]
case class Builtin(impl: BuiltinImpl) extends Native {
  override def isProc(): Boolean = true
  def inspect(): String = "<builtin>"
}

private[scalisp]
case class Macro(env: Env[Value], pattern: Pattern, code: Code) extends Native {
  override def isMeta(): Boolean = true
  def inspect(): String = "<macro>"
}

private[scalisp]
case class Syntax(impl: SyntaxImpl) extends Native {
  override def isMeta(): Boolean = true
  def inspect(): String = "<syntax>"
}

private[scalisp]
case class Vec(val payload: Array[Value]) extends Native {
  override def isVec(): Boolean = true
  def inspect(): String = (Sym("vec") Cons List(payload: _*)).inspect
}

private[scalisp]
case class Port(val impl: PortImpl) extends Native {
  override def isPort(): Boolean = true
  def inspect(): String = "<port>"
}
