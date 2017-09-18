package scalisp

import java.nio.ByteBuffer
import scala.util.control.NonFatal

trait Builtins {
  def put(context: Context, impl: NamedBuiltinImpl): Unit = {
    context.builtins(impl.name) = impl
  }

  def register(context: Context, args: Seq[String]): Unit = {
    put(context, BuiltinCons)

    put(context, BuiltinExit)
    put(context, BuiltinError)

    put(context, new BuiltinGensym)

    put(context, BuiltinCar)
    put(context, BuiltinCdr)

    put(context, BuiltinApply)

    put(context, BuiltinTestNum)
    put(context, BuiltinTestSym)
    put(context, BuiltinTestStr)
    put(context, BuiltinTestCons)
    put(context, BuiltinTestNil)
    put(context, BuiltinTestBool)
    put(context, BuiltinTestProc)
    put(context, BuiltinTestMeta)
    put(context, BuiltinTestPort)
    put(context, BuiltinTestVec)

    put(context, BuiltinAdd)
    put(context, BuiltinSub)
    put(context, BuiltinMul)
    put(context, BuiltinDiv)
    put(context, BuiltinMod)

    put(context, BuiltinEq)
    put(context, BuiltinLt)
    put(context, BuiltinGt)
    put(context, BuiltinLe)
    put(context, BuiltinGe)

    put(context, BuiltinCallCC)
    put(context, BuiltinNever)

    put(context, BuiltinStr)
    put(context, BuiltinStrRef)
    put(context, BuiltinStrBytesize)
    put(context, BuiltinStrConcat)
    put(context, BuiltinSubstr)
    put(context, BuiltinSymToStr)
    put(context, BuiltinNumToStr)
    put(context, BuiltinStrToNum)

    put(context, BuiltinVec)
    put(context, BuiltinVecMake)
    put(context, BuiltinVecRef)
    put(context, BuiltinVecLength)
    put(context, BuiltinVecSet)
    put(context, BuiltinVecCopy)

    // open
    put(context, BuiltinClose)
    // stdin, stdout, stderr

    put(context, BuiltinReadByte)
    put(context, BuiltinReadStr)
    put(context, BuiltinReadLine)
    put(context, BuiltinWriteByte)
    put(context, BuiltinWriteStr)
    put(context, BuiltinWriteLine)
    put(context, BuiltinFlush)

    put(context, new BuiltinArgs(args))

    put(context, BuiltinEval)
    put(context, BuiltinMacroExpand)
    put(context, BuiltinMacroExpandOnce)
  }
}

trait NamedBuiltinImpl extends BuiltinImpl {
  def name: String

  def takes[A](arity: String)(f: PartialFunction[Seq[Value], A])(args: Seq[Value]): A = {
    if (f isDefinedAt args) return f(args)
    throw new EvaluationError(s"$name takes $arity")
  }

  def takeNone = takes("no arguments") { case Seq() => {} } _
  def takeOne = takes("one argument") { case Seq(a) => a } _
  def takeTwo = takes("two arguments") { case Seq(a, b) => (a, b) } _
  def takeThree = takes("three arguments") { case Seq(a, b, c) => (a, b, c) } _
  def takeFive = takes("five arguments") { case Seq(a, b, c, d, e) => (a, b, c, d, e) } _

  def take[A](f: PartialFunction[Value, A])(name: String, v: Value): A = {
    if (f isDefinedAt v) return f(v)
    throw new EvaluationError(s"Expected $name but got ${v.inspect}")
  }

  def takeNum = take { case Num(n) => n } _
  def takeSym = take { case Sym(s) => s } _
  def takeStr = take { case Str(s) => s } _
  def takeCons = take { case a Cons b => (a, b) } _
  def takeList = take { case List(ls @ _*) => ls } _
  def takeVec = take { case Pure(Vec(arr)) => arr } _
  def takePort = take { case Pure(Port(p)) => p } _
  def takePortIn = take { case Pure(Port(p)) => p.in.getOrElse(throw new EvaluationError("port is not available for reading")) } _
  def takePortOut = take { case Pure(Port(p)) => p.out.getOrElse(throw new EvaluationError("port is not available for writing")) } _

  def readResult[A](r: Either[String, Option[A]])(convert: A => Value): Value = {
    r match {
      case Right(Some(v)) => Cons(True, convert(v))
      case Right(None) => Cons(True, Sym("eof"))
      case Left(e) => Cons(False, Str.fromString(e))
    }
  }

  def writeResult(r: Either[String, Int]): Value = {
    r match {
      case Right(i) => Cons(True, if (i == 0) Nil else Num(i))
      case Left(e) => Cons(False, Str.fromString(e))
    }
  }
}

object TryEval {
  def apply[A](body: => A): Either[String, A] = {
    try Right(body) catch {
      case e: VMException => throw e
      case NonFatal(e) => Left(Option(e.getMessage) getOrElse e.toString)
    }
  }
}

object BuiltinCons extends NamedBuiltinImpl {
  def name = "cons"
  def run(vm: VM, args: Seq[Value]) = {
    val (a, b) = takeTwo(args)
    vm.push(Cons(a, b))
  }
}

object BuiltinExit extends NamedBuiltinImpl {
  def name = "exit"
  def run(vm: VM, args: Seq[Value]) = {
    if (args.isEmpty) sys.exit()
    val n = takeNum("exitcode", takeOne(args))
    sys.exit(n.toInt)
  }
}

object BuiltinError extends NamedBuiltinImpl {
  def name = "error"
  def run(vm: VM, args: Seq[Value]) = {
    if (args.isEmpty) throw new EvaluationError("error called")
    val msg = takeStr("error message", takeOne(args))
    throw new EvaluationError(Str.decode(msg))
  }
}

final class BuiltinGensym extends NamedBuiltinImpl {
  private var id = 0
  def name = "gensym"
  def run(vm: VM, args: Seq[Value]) = {
    takeNone(args)
    id += 1
    vm.push(Sym("#sym." + id))
  }
}

object BuiltinCar extends NamedBuiltinImpl {
  def name = "car"
  def run(vm: VM, args: Seq[Value]) = {
    val (a, _) = takeCons("cons", takeOne(args))
    vm.push(a)
  }
}

object BuiltinCdr extends NamedBuiltinImpl {
  def name = "cdr"
  def run(vm: VM, args: Seq[Value]) = {
    val (_, a) = takeCons("cons", takeOne(args))
    vm.push(a)
  }
}

object BuiltinApply extends NamedBuiltinImpl {
  def name = "apply"
  def run(vm: VM, args: Seq[Value]) = {
    val (f, fargs) = takeTwo(args)
    val fseq = takeList("argument list", fargs)
    vm.app(f, fseq: _*)
  }
}

trait BuiltinTestImpl extends NamedBuiltinImpl {
  def test: PartialFunction[Value, Boolean]
  def run(vm: VM, args: Seq[Value]) = {
    val a = takeOne(args)
    val result = if (test.isDefinedAt(a)) test(a) else false
    vm.push(Bool(result))
  }
}

object BuiltinTestNum extends BuiltinTestImpl {
  def name = "num?"
  def test = { case Num(_) => true }
}

object BuiltinTestSym extends BuiltinTestImpl {
  def name = "sym?"
  def test = { case Sym(_) => true }
}

object BuiltinTestStr extends BuiltinTestImpl {
  def name = "str?"
  def test = { case Str(_) => true }
}

object BuiltinTestCons extends BuiltinTestImpl {
  def name = "cons?"
  def test = { case _ Cons _ => true }
}

object BuiltinTestNil extends BuiltinTestImpl {
  def name = "nil?"
  def test = { case Nil => true }
}

object BuiltinTestBool extends BuiltinTestImpl {
  def name = "bool?"
  def test = { case Bool(_) => true }
}

object BuiltinTestProc extends BuiltinTestImpl {
  def name = "proc?"
  def test = { case Pure(p) => p.isProc }
}

object BuiltinTestMeta extends BuiltinTestImpl {
  def name = "meta?"
  def test = { case Pure(p) => p.isMeta }
}

object BuiltinTestPort extends BuiltinTestImpl {
  def name = "port?"
  def test = { case Pure(p) => p.isPort }
}

object BuiltinTestVec extends BuiltinTestImpl {
  def name = "vec?"
  def test = { case Pure(p) => p.isVec }
}

trait BuiltinArithmeticImpl extends NamedBuiltinImpl {
  def zero: Option[Double]
  def one: Double => Double
  def fold: (Double, Double) => Double

  def run(vm: VM, ls: Seq[Value]): Unit = ls match {
    case Seq() => zero match {
      case Some(num) => vm.push(Num(num))
      case None => throw new EvaluationError(s"$name takes at least one argument")
    }
    case Seq(num) => vm.push(Num(one(takeNum("number", num))))
    case head +: tail => {
      val a = takeNum("number", head)
      val bs = tail.map(takeNum("number", _))
      vm.push(Num(bs.foldLeft(a)(fold)))
    }
  }
}

object BuiltinAdd extends BuiltinArithmeticImpl {
  def name = "+"
  def zero = Some(0)
  def one = +_
  def fold = _ + _
}

object BuiltinSub extends BuiltinArithmeticImpl {
  def name = "-"
  def zero = None
  def one = -_
  def fold = _ - _
}

object BuiltinMul extends BuiltinArithmeticImpl {
  def name = "*"
  def zero = Some(1)
  def one = +_
  def fold = _ * _
}

object BuiltinDiv extends BuiltinArithmeticImpl {
  def name = "/"
  def zero = None
  def one = 1 / _
  def fold = _ / _
}

object BuiltinMod extends BuiltinArithmeticImpl {
  def name = "%"
  def zero = None
  def one = +_
  def fold = _ % _
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

trait BuiltinCompareImpl extends NamedBuiltinImpl {
  def compare[A: Ordering](a: A, b: A): Boolean

  def compareAll[A: Ordering](a: A, bs: Seq[A]): Boolean = {
    (a +: bs).zip(bs).forall { case (l, r) => compare(l, r) }
  }

  def run(vm: VM, args: Seq[Value]): Unit = args match {
    case Seq() => vm.push(True)
    case Num(a) +: ls => {
      val bs = ls.map(takeNum("number", _))
      vm.push(Bool(compareAll(a, bs)))
    }
    case Str(a) +: ls => {
      import scala.math.Ordering.Implicits.seqDerivedOrdering
      val bs = ls.map(takeStr("string", _).toSeq)
      vm.push(Bool(compareAll(a.toSeq, bs)))
    }
    case _ => throw new EvaluationError(s"$name is only defined for strings or numbers")
  }
}

object BuiltinLt extends BuiltinCompareImpl {
  def name = "<"
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].lt(a, b)
}

object BuiltinGt extends BuiltinCompareImpl {
  def name = ">"
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].gt(a, b)
}

object BuiltinLe extends BuiltinCompareImpl {
  def name = "<="
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].lteq(a, b)
}

object BuiltinGe extends BuiltinCompareImpl {
  def name = ">="
  def compare[A: Ordering](a: A, b: A) = implicitly[Ordering[A]].gteq(a, b)
}

object BuiltinCallCC extends NamedBuiltinImpl {
  def name = "call/cc"
  def run(vm: VM, args: Seq[Value]) = {
    val f = takeOne(args)
    val cont = vm.captureCont()
    vm.app(f, cont)
  }
}

object BuiltinNever extends NamedBuiltinImpl {
  def name = "never"
  def run(vm: VM, args: Seq[Value]) = args match {
    case f +: args => vm.appNever(f, args: _*)
    case _ => throw new EvaluationError(s"$name takes at least one argument")
  }
}

object BuiltinStr extends NamedBuiltinImpl {
  def name = "str"
  def run(vm: VM, args: Seq[Value]) = {
    val bytes = args map { arg =>
      val num = takeNum("byte", arg)
      if (num < 0 || 255 < num) throw new EvaluationError("Each byte of string must be inside the range 0-255")
      num.toByte
    }
  vm.push(Str(bytes.toArray))
  }
}

object BuiltinStrRef extends NamedBuiltinImpl {
  def name = "str-ref"
  def run(vm: VM, args: Seq[Value]) = {
    val (s, i) = takeTwo(args)
    val str = takeStr("string", s)
    val index = takeNum("index", i).toInt
    vm.push(if (index < 0 || str.length <= index) Nil else Num(str(index).toInt & 0xff))
  }
}

object BuiltinStrBytesize extends NamedBuiltinImpl {
  def name = "str-bytesize"
  def run(vm: VM, args: Seq[Value]) = {
    val str = takeStr("string", takeOne(args))
    vm.push(Num(str.length))
  }
}

object BuiltinStrConcat extends NamedBuiltinImpl {
  def name = "str-concat"
  def run(vm: VM, args: Seq[Value]) = {
    val strs = args.map(takeStr("string", _))
    val buf = ByteBuffer.allocate(strs.map(_.length).sum)
    for (str <- strs) buf.put(str)
    vm.push(Str(buf.array))
  }
}

object BuiltinSubstr extends NamedBuiltinImpl {
  def name = "substr"
  def run(vm: VM, args: Seq[Value]) = {
    val (s, i, l) = takeThree(args)
    val str = takeStr("string", s)
    val index = takeNum("index", i).toInt
    val size = takeNum("size", l).toInt
    if (index < 0 || str.length < index+size) throw new EvaluationError("Index out of range")
    vm.push(Str(str.slice(index, index+size)))
  }
}

object BuiltinSymToStr extends NamedBuiltinImpl {
  def name = "sym->str"
  def run(vm: VM, args: Seq[Value]) = {
    val arg = takeOne(args)
    val s = takeSym("symbol", arg)
    vm.push(Str.fromString(s))
  }
}

object BuiltinNumToStr extends NamedBuiltinImpl {
  def name = "num->str"
  def run(vm: VM, args: Seq[Value]) = {
    val arg = takeOne(args)
    val n = takeNum("number", arg)
    vm.push(Str.fromString(Num(n).inspect))
  }
}

object BuiltinStrToNum extends NamedBuiltinImpl {
  def name = "str->num"
  def run(vm: VM, args: Seq[Value]) = {
    val arg = takeOne(args)
    val s = Str.decode(takeStr("string", arg))
    vm.push(TryEval(s.toDouble).fold(_ => Nil, Num.apply))
  }
}

object BuiltinVec extends NamedBuiltinImpl {
  def name = "vec"
  def run(vm: VM, args: Seq[Value]) = {
    vm.push(Pure(Vec(args.toArray)))
  }
}

object BuiltinVecMake extends NamedBuiltinImpl {
  def name = "vec-make"
  def run(vm: VM, args: Seq[Value]) = {
    val (i, init) = takeTwo(args)
    val length = takeNum("length", i).toInt
    val array = new Array[Value](length)
    for (i <- array.indices) array(i) = init
    vm.push(Pure(Vec(array)))
  }
}

object BuiltinVecRef extends NamedBuiltinImpl {
  def name = "vec-ref"
  def run(vm: VM, args: Seq[Value]) = {
    val (v, n) = takeTwo(args)
    val vec = takeVec("vector", v)
    val index = takeNum("index", n).toInt
    vm.push(if (index < 0 || vec.size <= index) Nil else vec(index))
  }
}

object BuiltinVecLength extends NamedBuiltinImpl {
  def name = "vec-length"
  def run(vm: VM, args: Seq[Value]) = {
    val v = takeOne(args)
    val vec = takeVec("vector", v)
    vm.push(Num(vec.length))
  }
}

object BuiltinVecSet extends NamedBuiltinImpl {
  def name = "vec-set!"
  def run(vm: VM, args: Seq[Value]) = {
    val (v, n, item) = takeThree(args)
    val vec = takeVec("vector", v)
    val index = takeNum("index", n).toInt
    if (index < 0 || vec.length <= index) throw new EvaluationError("Index out of range")
    vec(index) = item
    vm.push(Nil)
  }
}

object BuiltinVecCopy extends NamedBuiltinImpl {
  def name = "vec-copy!"
  def run(vm: VM, args: Seq[Value]) = {
    val (dest, destS, src, srcS, l) = takeFive(args)
    val destVec = takeVec("destination vector", dest)
    val destStart = takeNum("destination index", destS).toInt
    val srcVec = takeVec("source vector", src)
    val srcStart = takeNum("source index", srcS).toInt
    val length = takeNum("length", l).toInt

    if (0 <= srcStart && srcStart+length <= srcVec.length && 0 <= destStart && destStart+length <= destVec.length) {
      srcVec.drop(srcStart).copyToArray(destVec, destStart, length)
      vm.push(Nil)
    } else {
      throw new EvaluationError("Index out of range")
    }
  }
}

object BuiltinClose extends NamedBuiltinImpl {
  def name = "close"
  def run(vm: VM, args: Seq[Value]) = {
    val p = takeOne(args)
    val port = takePort("port", p)
    port.close() match {
      case Right(_) => vm.push(Cons(True, Nil))
      case Left(e) => vm.push(Cons(False, Str.fromString(e)))
    }
  }
}

class BuiltinPortImpl(val name: String, val portImpl: PortImpl) extends NamedBuiltinImpl {
  def run(vm: VM, args: Seq[Value]) = {
    takeNone(args)
    vm.push(Pure(Port(portImpl)))
  }
}

object BuiltinReadByte extends NamedBuiltinImpl {
  def name = "read-byte"
  def run(vm: VM, args: Seq[Value]) = {
    val p = takeOne(args)
    val r = takePortIn("port", p)
    vm.push(readResult(r.readByte)(v => Num(v.toInt & 0xff)))
  }
}

object BuiltinReadStr extends NamedBuiltinImpl {
  def name = "read-str"
  def run(vm: VM, args: Seq[Value]) = {
    val (s, p) = takeTwo(args)
    val size = takeNum("size", s).toInt
    val r = takePortIn("port", p)
    vm.push(readResult(r.readBytes(size))(Str.apply))
  }
}

object BuiltinReadLine extends NamedBuiltinImpl {
  def name = "read-line"
  def run(vm: VM, args: Seq[Value]) = {
    val p = takeOne(args)
    val r = takePortIn("port", p)
    vm.push(readResult(r.readLine)(Str.apply))
  }
}

object BuiltinWriteByte extends NamedBuiltinImpl {
  def name = "write-byte"
  def run(vm: VM, args: Seq[Value]) = {
    val (b, p) = takeTwo(args)
    val w = takePortOut("port", p)
    val byte = takeNum("byte", b).toByte
    vm.push(writeResult(w.writeByte(byte)))
  }
}

object BuiltinWriteStr extends NamedBuiltinImpl {
  def name = "write-str"
  def run(vm: VM, args: Seq[Value]) = {
    val (s, p) = takeTwo(args)
    val w = takePortOut("port", p)
    val str = takeStr("string", s)
    vm.push(writeResult(w.writeBytes(str)))
  }
}

object BuiltinWriteLine extends NamedBuiltinImpl {
  def name = "write-line"
  def run(vm: VM, args: Seq[Value]) = {
    val (s, p) = takeTwo(args)
    val w = takePortOut("port", p)
    val str = takeStr("string", s)
    vm.push(writeResult(w.writeLine(str)))
  }
}

object BuiltinFlush extends NamedBuiltinImpl {
  def name = "flush"
  def run(vm: VM, args: Seq[Value]) = {
    val p = takeOne(args)
    val w = takePortOut("port", p)
    vm.push(writeResult(w.flush))
  }
}

final class BuiltinArgs(val envArgs: Seq[String]) extends NamedBuiltinImpl {
  def name = "args"
  def run(vm: VM, args: Seq[Value]) = {
    takeNone(args)
    vm.push(List(envArgs.map(Str.fromString(_)): _*))
  }
}

object BuiltinEval extends NamedBuiltinImpl {
  def name = "eval"
  def run(vm: VM, args: Seq[Value]) = {
    val e = takeOne(args)
    vm.context(_.eval(e)) match {
      case Right(v) => vm.push(Cons(True, v))
      case Left(e) => vm.push(Cons(False, Str.fromString(e)))
    }
  }
}

trait BuiltinMacroExpandImpl extends NamedBuiltinImpl {
  def recurse: Boolean
  def run(vm: VM, args: Seq[Value]) = {
    val e = takeOne(args)
    vm.context(_.macroExpand(recurse, e)) match {
      case Right(v) => vm.push(Cons(True, v))
      case Left(e) => vm.push(Cons(False, Str.fromString(e)))
    }
  }
}

object BuiltinMacroExpand extends BuiltinMacroExpandImpl {
  def name = "macroexpand"
  def recurse = true
}

object BuiltinMacroExpandOnce extends BuiltinMacroExpandImpl {
  def name = "macroexpand-1"
  def recurse = false
}
