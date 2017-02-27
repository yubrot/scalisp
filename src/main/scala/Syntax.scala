package scalisp

class SyntaxEnv extends Env[Value](None) {
  private def register(impl: SyntaxCommon): Unit = define(impl.name, Pure(Syntax(impl)))

  register(SyntaxDef)
  register(SyntaxSet)
  register(SyntaxBegin)
  register(SyntaxIf)
  register(SyntaxFun)
  register(SyntaxMacro)
  register(SyntaxBuiltin)
  register(SyntaxQuote)
}

private[scalisp]
trait SyntaxCommon extends SyntaxImpl {
  def name: String
  def syntax: String
  def expand: Seq[Boolean]
  def body(compiler: Compiler): PartialFunction[Seq[Value], Unit]

  def expandArgs(me: MacroExpander, args: Seq[Value]): Seq[Value] = {
    args.map(Some(_)).zipAll(expand, None, true).collect { case (Some(arg), expand) =>
      if (expand) me.macroExpand(true, arg) else arg
    }
  }

  def interpret(compiler: Compiler, args: Seq[Value]): Unit = {
    def m = body(compiler)
    if (m.isDefinedAt(args))
      m(args)
    else
      throw new EvaluationError(s"Syntax error: expected ($name $syntax)")
  }
}

private[scalisp]
object SyntaxDef extends SyntaxCommon {
  def name = "def"
  def syntax = "sym x"
  def expand = Vector(false, true)
  def body(compiler: Compiler) = { case Seq(Sym(sym), x) =>
    compiler := x := Def(sym) := Ldc(Nil)
  }
}

private[scalisp]
object SyntaxSet extends SyntaxCommon {
  def name = "set!"
  def syntax = "sym x"
  def expand = Vector(false, true)
  def body(compiler: Compiler) = { case Seq(Sym(sym), x) =>
    compiler := x := Set(sym) := Ldc(Nil)
  }
}

private[scalisp]
object SyntaxBegin extends SyntaxCommon {
  def name = "begin"
  def syntax = "..."
  def expand = Vector()
  def body(compiler: Compiler) = { case args => compiler := args }
}

private[scalisp]
object SyntaxIf extends SyntaxCommon {
  def name = "if"
  def syntax = "cond then else"
  def expand = Vector(true, true, true)
  def body(compiler: Compiler) = { case Seq(c, t, e) =>
    val tb = compiler.block(_ := t := Leave)
    val eb = compiler.block(_ := e := Leave)
    compiler := c := Sel(tb, eb)
  }
}

private[scalisp]
object SyntaxFun extends SyntaxCommon {
  def name = "fun"
  def syntax = "pattern body..."
  def expand = Vector(false)
  def body(compiler: Compiler) = { case pat +: body =>
    val pattern = Pattern(pat)
    val block = compiler.block(_ := body := Leave)
    compiler := Ldf(pattern, block)
  }
}

private[scalisp]
object SyntaxMacro extends SyntaxCommon {
  def name = "macro"
  def syntax = "pattern body..."
  def expand = Vector(false)
  def body(compiler: Compiler) = { case pat +: body =>
    val pattern = Pattern(pat)
    val block = compiler.block(_ := body)
    compiler := Ldm(pattern, block)
  }
}

private[scalisp]
object SyntaxBuiltin extends SyntaxCommon {
  def name = "builtin"
  def syntax = "sym"
  def expand = Vector(false)
  def body(compiler: Compiler) = { case Seq(Sym(sym)) => compiler := Ldb(sym) }
}

private[scalisp]
object SyntaxQuote extends SyntaxCommon {
  def name = "quote"
  def syntax = "expr"
  def expand = Vector(false)
  def body(compiler: Compiler) = { case Seq(s) => compiler := Ldc(s) }
}
