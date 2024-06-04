package scalisp.core

type Value = Sexp[Native]

enum Native extends Inspect:
  case Fun(env: Env[Value], pattern: Pattern, code: Code)
  case Builtin(impl: BuiltinImpl)
  case Macro(env: Env[Value], pattern: Pattern, code: Code)
  case Syntax(impl: SyntaxImpl)
  case Vec(payload: Array[Value])

  def isProc: Boolean = this match
    case Fun(_, _, _) | Builtin(_) => true
    case _                         => false

  def isMeta: Boolean = this match
    case Macro(_, _, _) | Syntax(_) => true
    case _                          => false

  def isVec: Boolean = this match
    case Vec(_) => true
    case _      => false

  def inspect: String = this match
    case Fun(_, _, _)   => "<fun>"
    case Builtin(_)     => "<builtin>"
    case Macro(_, _, _) => "<macro>"
    case Syntax(_)      => "<syntax>"
    case Vec(payload)   => Sexp.Cons(Sexp.Sym("vec"), Sexp.List(payload: _*)).inspect
