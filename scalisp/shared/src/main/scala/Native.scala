package scalisp

abstract sealed class Native extends Inspect {
  def isProc(): Boolean = false
  def isMeta(): Boolean = false
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
