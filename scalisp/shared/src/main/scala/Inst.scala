package scalisp

abstract sealed class Inst

case class Ldc(constant: Value) extends Inst
case class Ldv(variable: String) extends Inst
case class Ldf(pattern: Pattern, code: Code) extends Inst
case class Ldm(pattern: Pattern, code: Code) extends Inst
case class Ldb(name: String) extends Inst
case class Sel(a: Code, b: Code) extends Inst
case class App(argc: Int) extends Inst
object Leave extends Inst
object Pop extends Inst
case class Def(variable: String) extends Inst
case class Set(variable: String) extends Inst
