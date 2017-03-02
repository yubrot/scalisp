package scalisp

import scala.annotation._
import scala.collection.mutable.{Map, ListBuffer}

case class InternalError(msg: String) extends Exception
case class EvaluationError(msg: String) extends Exception

trait SyntaxImpl {
  def expandArgs(macroExpander: MacroExpander, args: Seq[Value]): Seq[Value]
  def interpret(compiler: Compiler, args: Seq[Value]): Unit
}

trait BuiltinImpl {
  def run(vm: VM, args: Seq[Value]): Unit
}

trait MacroExpander {
  def macroExpand(recurse: Boolean, expr: Value): Value
}

class Compiler(private val compileEnv: Env[Value]) {
  private var buffer = new ListBuffer[Inst]

  def result(): Code = buffer.result

  def block(body: Compiler => Unit): Code = {
    val c = new Compiler(compileEnv)
    body(c)
    c.result
  }

  def :=(inst: Inst): Compiler = {
    buffer += inst
    this
  }

  def :=(value: Value): Compiler = {
    value match {
      case Sym(sym) => this := Ldv(sym)
      case List(f, args@ _*) => compileEnv.refer(f) match {
        case Some(Pure(Syntax(syntax))) => syntax.interpret(this, args)
        case _ =>
          this := f
          for (arg <- args) this := arg
          this := App(args.size)
      }
      case ls @(_ Cons _) => throw new EvaluationError("Improper list: " + ls.inspect)
      case s => this := Ldc(s)
    }
    this
  }

  def :=(seq: Seq[Value]): Compiler = {
    seq match {
      case Seq() => this := Ldc(Nil)
      case Seq(x) => this := x
      case x +: xs =>
        this := x
        for (x <- xs) this := Pop := x
    }
    this
  }
}

class VM(val context: Context, env: Env[Value], code: Code) {
  private[scalisp]
  val cont = new Cont(scala.Nil, env, code, scala.Nil)

  def push(v: Value): Unit = {
    cont.stack = v :: cont.stack
  }

  def pop(): Value = cont.stack match {
    case head :: rest =>
      cont.stack = rest
      head
    case _ => throw new InternalError("Inconsistent stack")
  }

  def enter(env: Env[Value], code: Code): Unit = {
    cont.code match {
      case Seq(Leave) => {} // tailcall: skip this frame
      case _ => cont.dump = (cont.env, cont.code) :: cont.dump
    }
    cont.env = env
    cont.code = code
  }

  def leave(): Unit = cont.dump match {
    case (env, code) :: rest =>
      cont.dump = rest
      cont.env = env
      cont.code = code
    case _ => throw new InternalError("Inconsistent dump")
  }

  def app(f: Value, args: Value*): Unit = f match {
    case Pure(Fun(fenv, fpat, fcode)) =>
      val env = new Env(Some(fenv))
      fpat.bind(env, args: _*)
      enter(env, fcode)
    case Pure(Builtin(builtin)) => builtin.run(this, args)
    case _ => throw new EvaluationError("Cannot call: " + f.inspect)
  }

  def captureCont(): Value = Pure(Builtin(cont.clone))

  def inst(inst: Inst): Unit = inst match {
    case Ldc(constant) => push(constant)
    case Ldv(variable) => push(cont.env.get(variable))
    case Ldf(pattern, code) => push(Pure(Fun(cont.env, pattern, code)))
    case Ldm(pattern, code) => push(Pure(Macro(cont.env, pattern, code)))
    case Ldb(name) => context.builtins.get(name) match {
      case Some(builtin) => push(Pure(Builtin(builtin)))
      case None => throw new EvaluationError("Unsupported builtin: " + name)
    }
    case Sel(a, b) =>
      val branch = if (pop().test) a else b
      enter(new Env(Some(cont.env)), branch)
    case App(argc) =>
      var args: List[Value] = scala.Nil
      for (_ <- 1 to argc) args = pop() :: args
      val f = pop()
      app(f, args: _*)
    case Leave => leave()
    case Pop => pop()
    case Def(name) =>
      val v = pop()
      cont.env.define(name, v)
    case Set(name) =>
      val v = pop()
      cont.env.set(name, v)
  }

  @tailrec final
  def run(): Value = cont.code match {
    case i +: rest =>
      cont.code = rest
      inst(i)
      run()
    case _ => pop()
  }
}

private[scalisp]
class Cont(
  var stack: List[Value],
  var env: Env[Value],
  var code: Code,
  var dump: List[(Env[Value], Code)]
) extends BuiltinImpl {
  override def clone(): Cont = new Cont(stack, env, code, dump)

  def overwrite(cont: Cont): Unit = {
    stack = cont.stack
    env = cont.env
    code = cont.code
    dump = cont.dump
  }

  def run(vm: VM, args: Seq[Value]): Unit = {
    if (args.size > 1) throw new EvaluationError("Multiple values are not implemented")
    vm.cont.overwrite(this)
    args match {
      case Seq() => vm.push(Nil)
      case Seq(x) => vm.push(x)
      case _ => throw new InternalError("args")
    }
  }
}

class Context extends MacroExpander {
  private val toplevel = new Env(Some(new SyntaxEnv))
  val builtins = Map.empty[String, BuiltinImpl]

  private def exec(env: Env[Value], code: Code): Value = new VM(this, env, code).run()

  def compile(expr: Value): Code = (new Compiler(toplevel) := expr).result

  def macroExpand(recurse: Boolean, expr: Value): Value = expr match {
    case List(m, args@ _*) => toplevel.refer(m) match {
      case Some(Pure(Macro(menv, mpat, mcode))) =>
        val env = new Env(Some(menv))
        mpat.bind(env, args: _*)
        val s = exec(env, mcode)
        if (recurse) macroExpand(true, s) else s
      case Some(Pure(Syntax(syntax))) =>
        if (recurse) {
          val e = m +: syntax.expandArgs(this, args)
          List(e: _*)
        } else expr
      case _ => if (recurse) macroExpandChildren(expr) else expr
    }
    case _ => if (recurse) macroExpandChildren(expr) else expr
  }

  private def macroExpandChildren(expr: Value): Value = expr match {
    case a Cons b => macroExpand(true, a) Cons macroExpandChildren(b)
    case _ => expr
  }

  def eval(expr: Value): Value = {
    var s = macroExpand(true, expr)
    val code = compile(s)
    exec(toplevel, code)
  }

  def apply[A](body: Context => A): Either[String, A] = {
    try {
      Right(body(this))
    } catch {
      case e: UndefinedVariable => Left("Undefined variable: " + e.name)
      case e: EvaluationError => Left("Evaluation error: " + e.msg)
      case e: InternalError => Left("Internal error: " + e.msg)
    }
  }
}
