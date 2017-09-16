package scalisp

import java.nio.ByteBuffer

object Builtins {
  def register(context: Context): Unit = {
    def register(impl: NamedBuiltinImpl): Unit = context.builtins(impl.name) = impl

    register(BuiltinCons)

    register(BuiltinExit)
    register(BuiltinError)

    register(new BuiltinGensym)

    register(BuiltinCar)
    register(BuiltinCdr)

    register(BuiltinApply)

    register(BuiltinTestNum)
    register(BuiltinTestSym)
    register(BuiltinTestStr)
    register(BuiltinTestCons)
    register(BuiltinTestNil)
    register(BuiltinTestBool)
    register(BuiltinTestProc)
    register(BuiltinTestMeta)

    register(BuiltinAdd)
    register(BuiltinSub)
    register(BuiltinMul)
    register(BuiltinDiv)
    register(BuiltinMod)

    register(BuiltinConcat)
    register(BuiltinLength)

    register(BuiltinEq)
    register(BuiltinLt)
    register(BuiltinGt)
    register(BuiltinLe)
    register(BuiltinGe)

    register(BuiltinCallCC)

    register(BuiltinEval)
    register(BuiltinMacroExpandAll)
    register(BuiltinMacroExpandOnce)

    register(BuiltinPrint)
    register(BuiltinNewline)

    register(BuiltinInspect)
  }
}

trait NamedBuiltinImpl extends BuiltinImpl {
  def name: String

  def extractNumbers(ls: Seq[Value]): Seq[Double] = ls map {
    case Num(num) => num
    case s => throw new EvaluationError(s"Operator $name takes number arguments: ${s.inspect}")
  }

  def extractStrings(ls: Seq[Value]): Seq[Array[Byte]] = ls map {
    case Str(str) => str
    case s => throw new EvaluationError(s"Operator $name takes string arguments: ${s.inspect}")
  }
}

trait BuiltinCommon extends NamedBuiltinImpl {
  def args: String
  def body(vm: VM): PartialFunction[Seq[Value], Unit]

  def run(vm: VM, ls: Seq[Value]): Unit = {
    val f = body(vm)
    if (f.isDefinedAt(ls))
      f(ls)
    else
      throw new EvaluationError(s"Builtin function $name takes $args")
  }
}

object BuiltinCons extends BuiltinCommon {
  def name = "cons"
  def args = "2 arguments"
  def body(vm: VM) = { case Seq(a, b) => vm.push(a Cons b) }
}

object BuiltinExit extends BuiltinCommon {
  def name = "exit"
  def args = "a number argument"
  def body(vm: VM) = {
    case Seq() => sys.exit()
    case Seq(Num(n)) => sys.exit(n.toInt)
  }
}

object BuiltinError extends BuiltinCommon {
  def name = "error"
  def args = "a string argument"
  def body(vm: VM) = { case Seq(str @ Str(_)) => throw new EvaluationError(str.toString) }
}

final class BuiltinGensym extends BuiltinCommon {
  private var id = 0
  def name = "gensym"
  def args = "no arguments"
  def body(vm: VM) = { case Seq() =>
    id += 1
    vm.push(Sym("#sym." + id))
  }
}

object BuiltinCar extends BuiltinCommon {
  def name = "car"
  def args = "a cons"
  def body(vm: VM) = { case Seq(a Cons _) => vm.push(a) }
}

object BuiltinCdr extends BuiltinCommon {
  def name = "cdr"
  def args = "a cons"
  def body(vm: VM) = { case Seq(_ Cons b) => vm.push(b) }
}

object BuiltinApply extends BuiltinCommon {
  def name = "apply"
  def args = "2 arguments"
  def body(vm: VM) = { case Seq(f, List(args@ _*)) => vm.app(f, args: _*) }
}

trait BuiltinTest extends BuiltinCommon {
  def args = "1 argument"
  def test: PartialFunction[Value, Boolean]
  def body(vm: VM) = { case Seq(a) =>
    val result = if (test.isDefinedAt(a)) test(a) else false
    vm.push(Bool(result))
  }
}

object BuiltinTestNum extends BuiltinTest {
  def name = "num?"
  def test = { case Num(_) => true }
}

object BuiltinTestSym extends BuiltinTest {
  def name = "sym?"
  def test = { case Sym(_) => true }
}

object BuiltinTestStr extends BuiltinTest {
  def name = "str?"
  def test = { case Str(_) => true }
}

object BuiltinTestCons extends BuiltinTest {
  def name = "cons?"
  def test = { case _ Cons _ => true }
}

object BuiltinTestNil extends BuiltinTest {
  def name = "nil?"
  def test = { case Nil => true }
}

object BuiltinTestBool extends BuiltinTest {
  def name = "bool?"
  def test = { case Bool(_) => true }
}

object BuiltinTestProc extends BuiltinTest {
  def name = "proc?"
  def test = { case Pure(p) => p.isProc }
}

object BuiltinTestMeta extends BuiltinTest {
  def name = "meta?"
  def test = { case Pure(p) => p.isMeta }
}

trait BuiltinArithmetic extends NamedBuiltinImpl {
  def zero: Option[Double]
  def one: Double => Double
  def fold: (Double, Double) => Double

  def run(vm: VM, ls: Seq[Value]): Unit = extractNumbers(ls) match {
    case Seq() => zero match {
      case Some(num) => vm.push(Num(num))
      case None => throw new EvaluationError(s"Operator $name takes at least one argument")
    }
    case Seq(num) => vm.push(Num(one(num)))
    case head +: tail => vm.push(Num(tail.foldLeft(head)(fold)))
  }
}

object BuiltinAdd extends BuiltinArithmetic {
  def name = "+"
  def zero = Some(0)
  def one = +_
  def fold = _ + _
}

object BuiltinSub extends BuiltinArithmetic {
  def name = "-"
  def zero = None
  def one = -_
  def fold = _ - _
}

object BuiltinMul extends BuiltinArithmetic {
  def name = "*"
  def zero = Some(1)
  def one = +_
  def fold = _ * _
}

object BuiltinDiv extends BuiltinArithmetic {
  def name = "/"
  def zero = None
  def one = 1 / _
  def fold = _ / _
}

object BuiltinMod extends BuiltinArithmetic {
  def name = "%"
  def zero = None
  def one = +_
  def fold = _ % _
}

object BuiltinConcat extends NamedBuiltinImpl {
  def name = "concat"

  def run(vm: VM, ls: Seq[Value]): Unit = {
    val strs = extractStrings(ls)
    val buf = ByteBuffer.allocate(strs.map(_.length).sum)
    for (str <- strs) buf.put(str)
    vm.push(Str(buf.array))
  }
}

object BuiltinLength extends BuiltinCommon {
  def name = "length"
  def args = "a string argument"
  def body(vm: VM) = { case Seq(Str(str)) => vm.push(Num(str.length)) }
}

object BuiltinEq extends NamedBuiltinImpl {
  def name = "="

  def run(vm: VM, ls: Seq[Value]): Unit = ls match {
    case Seq() => vm.push(True)
    case head +: tail => vm.push(Bool(tail.forall(equal(head, _))))
  }

  def equal(a: Value, b: Value): Boolean = (a, b) match {
    case (Num(a), Num(b)) => a == b
    case (Sym(a), Sym(b)) => a == b
    case (Str(a), Str(b)) => a.toSeq == b.toSeq
    case (Cons(a, a2), Cons(b, b2)) => equal(a, b) && equal(a2, b2)
    case (Nil, Nil) => true
    case (Bool(a), Bool(b)) => a == b
    case (_, _) => false
  }
}

trait BuiltinCompare extends NamedBuiltinImpl {
  def compare[A: Ordering](a: A, b: A): Boolean

  def compareAll[A: Ordering](a: A, bs: Seq[A]): Boolean = {
    (a +: bs).zip(bs).forall { case (l, r) => compare(l, r) }
  }

  def run(vm: VM, ls: Seq[Value]): Unit = ls match {
    case Seq() => vm.push(True)
    case Num(a) +: ls => {
      val bs = extractNumbers(ls)
      vm.push(Bool(compareAll(a, bs)))
    }
    case Str(a) +: ls => {
      import scala.math.Ordering.Implicits.seqDerivedOrdering
      val bs = extractStrings(ls).map(_.toSeq)
      vm.push(Bool(compareAll(a.toSeq, bs)))
    }
    case _ => throw new EvaluationError(s"Operator $name is only defined for strings and numbers")
  }
}

object BuiltinLt extends BuiltinCompare {
  def name = "<"
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].lt(a, b)
}

object BuiltinGt extends BuiltinCompare {
  def name = ">"
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].gt(a, b)
}

object BuiltinLe extends BuiltinCompare {
  def name = "<="
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].lteq(a, b)
}

object BuiltinGe extends BuiltinCompare {
  def name = ">="
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].gteq(a, b)
}

object BuiltinCallCC extends BuiltinCommon {
  def name = "call/cc"
  def args = "one argument"
  def body(vm: VM) = { case Seq(f) =>
    val cont = vm.captureCont()
    vm.app(f, cont)
  }
}

object BuiltinEval extends BuiltinCommon {
  def name = "eval"
  def args = "an expression"
  def body(vm: VM) = { case Seq(e) =>
    vm.context(_.eval(e)) match {
      case Right(v) => vm.push(Cons(True, v))
      case Left(e) => vm.push(Cons(False, Str(e)))
    }
  }
}

trait BuiltinMacroExpand extends BuiltinCommon {
  def args = "an expression"
  def recurse: Boolean
  def body(vm: VM) = { case Seq(e) =>
    vm.context(_.macroExpand(recurse, e)) match {
      case Right(v) => vm.push(Cons(True, v))
      case Left(e) => vm.push(Cons(False, Str(e)))
    }
  }
}

object BuiltinMacroExpandAll extends BuiltinMacroExpand {
  def name = "macroexpand"
  def recurse = true
}

object BuiltinMacroExpandOnce extends BuiltinMacroExpand {
  def name = "macroexpand-1"
  def recurse = false
}

object BuiltinPrint extends NamedBuiltinImpl {
  def name = "print"
  def run(vm: VM, ls: Seq[Value]): Unit = {
    ls foreach {
      case str @ Str(_) => print(str.toString)
      case s => throw new EvaluationError("Cannot print non-string argument: " + s.inspect)
    }
    vm.push(Nil)
  }
}

object BuiltinNewline extends BuiltinCommon {
  def name = "newline"
  def args = "no arguments"
  def body(vm: VM) = { case Seq() =>
    println()
    vm.push(Nil)
  }
}

object BuiltinInspect extends BuiltinCommon {
  def name = "inspect"
  def args = "one argument"
  def body(vm: VM) = { case Seq(a) => vm.push(Str(a.inspect)) }
}
