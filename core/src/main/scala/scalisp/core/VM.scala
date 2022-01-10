package scalisp.core

import scala.annotation._
import scala.collection.mutable.{Map, ListBuffer}

abstract sealed class VMException extends Exception
class InternalError(val msg: String) extends VMException
class EvaluationError(val msg: String) extends VMException

trait SyntaxImpl:
  def expandArgs(macroExpander: MacroExpander, args: Seq[Value]): Seq[Value]
  def compile(compiler: Compiler, args: Seq[Value]): Unit

trait BuiltinImpl:
  def run(vm: VM, args: Seq[Value]): Unit

trait MacroExpander:
  def macroExpand(recurse: Boolean, expr: Value): Value

class Compiler(private val compileEnv: Env[Value]):
  private var buffer = ListBuffer[Inst]()

  def result: Code = buffer.result

  def block(body: Compiler => Unit): Code =
    val c = scalisp.core.Compiler(compileEnv)
    body(c)
    c.result

  def :=(inst: Inst): Compiler =
    buffer += inst
    this

  def :=(value: Value): Compiler =
    value match
      case Sexp.Sym(sym) => this := Inst.Ldv(sym)
      case Sexp.List(f, args @ _*) =>
        compileEnv.refer(f) match
          case Some(Sexp.Pure(Native.Syntax(impl))) => impl.compile(this, args)
          case _ =>
            this := f
            for arg <- args do this := arg
            this := Inst.App(args.size)
      case ls @ Sexp.Cons(_, _) =>
        throw EvaluationError("Improper list: " + ls.inspect)
      case s =>
        this := Inst.Ldc(s)
    this

  def :=(seq: Seq[Value]): Compiler =
    seq match
      case Seq()  => this := Inst.Ldc(Sexp.Nil)
      case Seq(x) => this := x
      case x +: xs =>
        this := x
        for x <- xs do this := Inst.Pop := x
    this

class VM(val context: Context, env: Env[Value], code: Code):
  private[core] val cont = Cont(scala.Nil, env, code, scala.Nil)

  def push(v: Value): Unit =
    cont.stack = v :: cont.stack

  def pop(): Value = cont.stack match
    case head :: rest =>
      cont.stack = rest
      head
    case _ => throw scalisp.core.InternalError("Inconsistent stack")

  def enter(env: Env[Value], code: Code): Unit =
    cont.code match
      case Seq(Inst.Leave) => {} // tailcall: skip this frame
      case _               => cont.dump = (cont.env, cont.code) :: cont.dump
    cont.env = env
    cont.code = code

  def leave(): Unit = cont.dump match
    case (env, code) :: rest =>
      cont.dump = rest
      cont.env = env
      cont.code = code
    case _ => throw scalisp.core.InternalError("Inconsistent dump")

  def app(f: Value, args: Value*): Unit = f match
    case Sexp.Pure(Native.Fun(fenv, fpat, fcode)) =>
      val env = Env(Some(fenv))
      fpat.bind(env, args: _*)
      enter(env, fcode)
    case Sexp.Pure(Native.Builtin(impl)) =>
      impl.run(this, args)
    case _ =>
      throw EvaluationError("Cannot call: " + f.inspect)

  def appNever(f: Value, args: Value*): Unit =
    cont.stack = scala.Nil
    cont.code = Seq(Inst.Leave)
    cont.dump = scala.Nil
    app(f, args: _*)

  def captureCont(): Value = Sexp.Pure(Native.Builtin(cont.clone))

  def inst(inst: Inst): Unit = inst match
    case Inst.Ldc(constant)      => push(constant)
    case Inst.Ldv(variable)      => push(cont.env.get(variable))
    case Inst.Ldf(pattern, code) => push(Sexp.Pure(Native.Fun(cont.env, pattern, code)))
    case Inst.Ldm(pattern, code) => push(Sexp.Pure(Native.Macro(cont.env, pattern, code)))
    case Inst.Ldb(name) =>
      context.builtins.get(name) match
        case Some(builtin) => push(Sexp.Pure(Native.Builtin(builtin)))
        case None          => throw EvaluationError("Unsupported builtin: " + name)
    case Inst.Sel(a, b) =>
      val branch = if pop().test then a else b
      enter(Env(Some(cont.env)), branch)
    case Inst.App(argc) =>
      var args: List[Value] = scala.Nil
      for _ <- 1 to argc do args = pop() :: args
      val f = pop()
      app(f, args: _*)
    case Inst.Leave => leave()
    case Inst.Pop   => pop()
    case Inst.Def(name) =>
      val v = pop()
      cont.env.define(name, v)
    case Inst.Set(name) =>
      val v = pop()
      cont.env.set(name, v)

  @tailrec final def run(): Value = cont.code match
    case i +: rest =>
      cont.code = rest
      inst(i)
      run()
    case _ => pop()

class Cont(
    var stack: List[Value],
    var env: Env[Value],
    var code: Code,
    var dump: List[(Env[Value], Code)]
) extends BuiltinImpl:
  override def clone(): Cont = Cont(stack, env, code, dump)

  def overwrite(cont: Cont): Unit =
    stack = cont.stack
    env = cont.env
    code = cont.code
    dump = cont.dump

  def run(vm: VM, args: Seq[Value]): Unit =
    if args.size > 1 then throw EvaluationError("Multiple values are not implemented")
    vm.cont.overwrite(this)
    args match
      case Seq()  => vm.push(Sexp.Nil)
      case Seq(x) => vm.push(x)
      case _      => throw scalisp.core.InternalError("args")

class Context extends MacroExpander:
  private val toplevel = Env(Some(Syntax.install(Env(None))))
  val builtins = Map.empty[String, BuiltinImpl]

  private def exec(env: Env[Value], code: Code): Value =
    VM(this, env, code).run()

  def compile(expr: Value): Code =
    (scalisp.core.Compiler(toplevel) := expr).result

  def macroExpand(recurse: Boolean, expr: Value): Value = expr match
    case Sexp.List(m, args @ _*) =>
      toplevel.refer(m) match
        case Some(Sexp.Pure(Native.Macro(menv, mpat, mcode))) =>
          val env = Env(Some(menv))
          mpat.bind(env, args: _*)
          val s = exec(env, mcode)
          if recurse then macroExpand(true, s) else s
        case Some(Sexp.Pure(Native.Syntax(impl))) =>
          if recurse then
            val e = m +: impl.expandArgs(this, args)
            Sexp.List(e: _*)
          else expr
        case _ => if recurse then macroExpandChildren(expr) else expr
    case _ => if recurse then macroExpandChildren(expr) else expr

  private def macroExpandChildren(expr: Value): Value = expr match
    case Sexp.Cons(a, b) => Sexp.Cons(macroExpand(true, a), macroExpandChildren(b))
    case _               => expr

  def eval(expr: Value): Value =
    var s = macroExpand(true, expr)
    val code = compile(s)
    exec(toplevel, code)

  def apply[A](body: Context => A): Either[String, A] =
    try Right(body(this))
    catch
      case e: UndefinedVariable => Left("Undefined variable: " + e.name)
      case e: EvaluationError   => Left("Evaluation error: " + e.msg)
      case e: InternalError     => Left("Internal error: " + e.msg)
