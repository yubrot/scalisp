package scalisp.core

object Syntax:
  def install(env: Env[Value]): Env[Value] =
    def register(impl: CommonSyntaxImpl): Unit = env.define(impl.name, Sexp.Pure(Native.Syntax(impl)))
    register(SyntaxDef)
    register(SyntaxSet)
    register(SyntaxBegin)
    register(SyntaxIf)
    register(SyntaxFun)
    register(SyntaxMacro)
    register(SyntaxBuiltin)
    register(SyntaxQuote)
    env

trait CommonSyntaxImpl extends SyntaxImpl:
  def name: String
  def syntax: String
  def expand: Seq[Boolean]
  def body(compiler: Compiler): PartialFunction[Seq[Value], Unit]

  def expandArgs(me: MacroExpander, args: Seq[Value]): Seq[Value] =
    args
      .map(Some.apply)
      .zipAll(expand, None, true)
      .collect { case (Some(arg), expand) => if expand then me.macroExpand(true, arg) else arg }

  def compile(compiler: Compiler, args: Seq[Value]): Unit =
    val m = body(compiler)
    if m.isDefinedAt(args) then m(args)
    else throw EvaluationError(s"Syntax error: expected ($name $syntax)")

object SyntaxDef extends CommonSyntaxImpl:
  def name = "def"
  def syntax = "sym x"
  def expand = Seq(false, true)
  def body(compiler: Compiler) = { case Seq(Sexp.Sym(sym), x) => compiler := x := Inst.Def(sym) := Inst.Ldc(Sexp.Nil) }

object SyntaxSet extends CommonSyntaxImpl:
  def name = "set!"
  def syntax = "sym x"
  def expand = Seq(false, true)
  def body(compiler: Compiler) = { case Seq(Sexp.Sym(sym), x) => compiler := x := Inst.Set(sym) := Inst.Ldc(Sexp.Nil) }

object SyntaxBegin extends CommonSyntaxImpl:
  def name = "begin"
  def syntax = "..."
  def expand = Seq()
  def body(compiler: Compiler) = { case args => compiler := args }

object SyntaxIf extends CommonSyntaxImpl:
  def name = "if"
  def syntax = "cond then else"
  def expand = Seq(true, true, true)
  def body(compiler: Compiler) = { case Seq(c, t, e) =>
    val tb = compiler.block { _ := t := Inst.Leave }
    val eb = compiler.block { _ := e := Inst.Leave }
    compiler := c := Inst.Sel(tb, eb)
  }

object SyntaxFun extends CommonSyntaxImpl:
  def name = "fun"
  def syntax = "pattern body..."
  def expand = Seq(false)
  def body(compiler: Compiler) = { case pat +: body =>
    val pattern = Pattern(pat)
    val block = compiler.block { _ := body := Inst.Leave }
    compiler := Inst.Ldf(pattern, block)
  }

object SyntaxMacro extends CommonSyntaxImpl:
  def name = "macro"
  def syntax = "pattern body..."
  def expand = Seq(false)
  def body(compiler: Compiler) = { case pat +: body =>
    val pattern = Pattern(pat)
    val block = compiler.block { _ := body }
    compiler := Inst.Ldm(pattern, block)
  }

object SyntaxBuiltin extends CommonSyntaxImpl:
  def name = "builtin"
  def syntax = "sym"
  def expand = Seq(false)
  def body(compiler: Compiler) = { case Seq(Sexp.Sym(sym)) => compiler := Inst.Ldb(sym) }

object SyntaxQuote extends CommonSyntaxImpl:
  def name = "quote"
  def syntax = "expr"
  def expand = Seq(false)
  def body(compiler: Compiler) = { case Seq(s) => compiler := Inst.Ldc(s) }
