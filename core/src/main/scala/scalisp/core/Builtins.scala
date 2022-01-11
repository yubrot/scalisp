package scalisp.core

import java.io.{InputStream, BufferedInputStream, OutputStream, BufferedOutputStream}
import java.nio.ByteBuffer
import java.nio.file.{FileSystems, Files}
import scala.util.control.NonFatal

object Builtins:
  def register(context: Context, args: Seq[String]): Unit =
    def put(context: Context, impl: CommonBuiltinImpl): Unit = context.builtins(impl.name) = impl

    put(context, BuiltinCons)

    put(context, BuiltinExit)
    put(context, BuiltinError)

    put(context, BuiltinGensym())

    put(context, BuiltinCar)
    put(context, BuiltinCdr)

    put(context, BuiltinApply)

    put(context, builtinTestNum)
    put(context, builtinTestSym)
    put(context, builtinTestStr)
    put(context, builtinTestCons)
    put(context, builtinTestNil)
    put(context, builtinTestBool)
    put(context, builtinTestProc)
    put(context, builtinTestMeta)
    put(context, builtinTestPort)
    put(context, builtinTestVec)

    put(context, builtinAdd)
    put(context, builtinSub)
    put(context, builtinMul)
    put(context, builtinDiv)
    put(context, builtinMod)

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

    put(context, BuiltinOpen)
    put(context, BuiltinClose)
    put(context, BuiltinStdin)
    put(context, BuiltinStdout)
    put(context, BuiltinStderr)

    put(context, BuiltinReadByte)
    put(context, BuiltinReadStr)
    put(context, BuiltinReadLine)
    put(context, BuiltinWriteByte)
    put(context, BuiltinWriteStr)
    put(context, BuiltinWriteLine)
    put(context, BuiltinFlush)

    put(context, BuiltinArgs(args))

    put(context, BuiltinEval)
    put(context, BuiltinMacroExpand)
    put(context, BuiltinMacroExpandOnce)

object TryEval:
  def apply[A](body: => A): Either[String, A] =
    try Right(body)
    catch
      case e: VMException => throw e
      case NonFatal(e)    => Left(Option(e.getMessage) getOrElse e.toString)

trait CommonBuiltinImpl extends BuiltinImpl:
  def name: String

  def take[T: MatchArgs](args: Seq[Value]): T =
    val matcher = summon[MatchArgs[T]]
    matcher.matchArgs(args) match
      case Some(r) => r
      case None    => throw EvaluationError(s"Expected (${(name +: matcher.signature).mkString(" ")})")

object BuiltinCons extends CommonBuiltinImpl:
  def name = "cons"
  def run(vm: VM, args: Seq[Value]) =
    val (a, b) = take[(Value, Value)](args)
    vm.push(Sexp.Cons(a, b))

object BuiltinExit extends CommonBuiltinImpl:
  def name = "exit"
  def run(vm: VM, args: Seq[Value]) =
    if args.isEmpty then sys.exit()
    sys.exit(take[Int](args))

object BuiltinError extends CommonBuiltinImpl:
  def name = "error"
  def run(vm: VM, args: Seq[Value]) =
    if args.isEmpty then throw EvaluationError("error called")
    val msg = take[String](args)
    throw EvaluationError(msg)

class BuiltinGensym extends CommonBuiltinImpl:
  private var id = 0
  def name = "gensym"
  def run(vm: VM, args: Seq[Value]) =
    take[Unit](args)
    id += 1
    vm.push(Sexp.Sym("#sym." + id))

object BuiltinCar extends CommonBuiltinImpl:
  def name = "car"
  def run(vm: VM, args: Seq[Value]) =
    val cons = take[Sexp.Cons[Native]](args)
    vm.push(cons.car)

object BuiltinCdr extends CommonBuiltinImpl:
  def name = "cdr"
  def run(vm: VM, args: Seq[Value]) =
    val cons = take[Sexp.Cons[Native]](args)
    vm.push(cons.cdr)

object BuiltinApply extends CommonBuiltinImpl:
  def name = "apply"
  def run(vm: VM, args: Seq[Value]) =
    val (f, fargs) = take[(Value, Seq[Value])](args)
    vm.app(f, fargs: _*)

class BuiltinTestImpl(val name: String)(test: PartialFunction[Value, Boolean]) extends CommonBuiltinImpl:
  def run(vm: VM, args: Seq[Value]) =
    val a = take[Value](args)
    val result = if test isDefinedAt a then test(a) else false
    vm.push(Sexp.Bool(result))

lazy val builtinTestNum = BuiltinTestImpl("num?") { case Sexp.Num(_) => true }
lazy val builtinTestSym = BuiltinTestImpl("sym?") { case Sexp.Sym(_) => true }
lazy val builtinTestStr = BuiltinTestImpl("str?") { case Sexp.Str(_) => true }
lazy val builtinTestCons = BuiltinTestImpl("cons?") { case Sexp.Cons(_, _) => true }
lazy val builtinTestNil = BuiltinTestImpl("nil?") { case Sexp.Nil => true }
lazy val builtinTestBool = BuiltinTestImpl("bool?") { case Sexp.Bool(_) => true }
lazy val builtinTestProc = BuiltinTestImpl("proc?") { case Sexp.Pure(p) => p.isProc }
lazy val builtinTestMeta = BuiltinTestImpl("meta?") { case Sexp.Pure(p) => p.isMeta }
lazy val builtinTestPort = BuiltinTestImpl("port?") { case Sexp.Pure(p) => p.isPort }
lazy val builtinTestVec = BuiltinTestImpl("vec?") { case Sexp.Pure(p) => p.isVec }

class BuiltinArithmeticImpl(
    val name: String,
    zero: Option[Double],
    one: Double => Double,
    fold: (Double, Double) => Double
) extends CommonBuiltinImpl:
  def run(vm: VM, args: Seq[Value]): Unit = args.length match
    case 0 =>
      zero match
        case Some(num) => vm.push(Sexp.Num(num))
        case None      => throw EvaluationError(s"$name takes at least one argument")
    case 1 =>
      val num = take[Double](args)
      vm.push(Sexp.Num(one(num)))
    case _ =>
      val (a, Rest(bs)) = take[(Double, Rest[Double])](args)
      vm.push(Sexp.Num(bs.foldLeft(a)(fold)))

lazy val builtinAdd = BuiltinArithmeticImpl("+", Some(0), +_, _ + _)
lazy val builtinSub = BuiltinArithmeticImpl("-", None, -_, _ - _)
lazy val builtinMul = BuiltinArithmeticImpl("*", Some(1), +_, _ * _)
lazy val builtinDiv = BuiltinArithmeticImpl("/", None, 1 / _, _ / _)
lazy val builtinMod = BuiltinArithmeticImpl("%", None, +_, _ % _)

object BuiltinEq extends CommonBuiltinImpl:
  def name = "="

  def run(vm: VM, ls: Seq[Value]): Unit = ls match
    case Seq()        => vm.push(Sexp.True)
    case head +: tail => vm.push(Sexp.Bool(tail.forall(equal(head, _))))

  def equal(a: Value, b: Value): Boolean = (a, b) match
    case (Sexp.Num(a), Sexp.Num(b))           => a == b
    case (Sexp.Sym(a), Sexp.Sym(b))           => a == b
    case (Sexp.Str(a), Sexp.Str(b))           => a.toSeq == b.toSeq
    case (Sexp.Cons(a, a2), Sexp.Cons(b, b2)) => equal(a, b) && equal(a2, b2)
    case (Sexp.Nil, Sexp.Nil)                 => true
    case (Sexp.Bool(a), Sexp.Bool(b))         => a == b
    case (_, _)                               => false

trait BuiltinCompareImpl extends CommonBuiltinImpl:
  def compare[A: Ordering](a: A, b: A): Boolean

  def compareAll[A: Ordering](a: A, bs: Seq[A]): Boolean =
    (a +: bs).zip(bs).forall { case (l, r) => compare(l, r) }

  def run(vm: VM, args: Seq[Value]): Unit = args match
    case Seq() => vm.push(Sexp.True)
    case Sexp.Num(a) +: ls =>
      val Rest(bs) = take[Rest[Double]](ls)
      vm.push(Sexp.Bool(compareAll(a, bs)))
    case Sexp.Str(a) +: ls =>
      import math.Ordering.Implicits.seqOrdering
      val Rest(bs) = take[Rest[Sexp.Str]](ls)
      vm.push(Sexp.Bool(compareAll(a.toSeq, bs.map(_.data.toSeq))))
    case _ => throw EvaluationError(s"$name is only defined for strings or numbers")

object BuiltinLt extends BuiltinCompareImpl:
  def name = "<"
  def compare[A: Ordering](a: A, b: A) = summon[Ordering[A]].lt(a, b)

object BuiltinGt extends BuiltinCompareImpl:
  def name = ">"
  def compare[A: Ordering](a: A, b: A) = summon[Ordering[A]].gt(a, b)

object BuiltinLe extends BuiltinCompareImpl:
  def name = "<="
  def compare[A: Ordering](a: A, b: A) = summon[Ordering[A]].lteq(a, b)

object BuiltinGe extends BuiltinCompareImpl:
  def name = ">="
  def compare[A: Ordering](a: A, b: A) = summon[Ordering[A]].gteq(a, b)

object BuiltinCallCC extends CommonBuiltinImpl:
  def name = "call/cc"
  def run(vm: VM, args: Seq[Value]) =
    val f = take[Value](args)
    val cont = vm.captureCont()
    vm.app(f, cont)

object BuiltinNever extends CommonBuiltinImpl:
  def name = "never"
  def run(vm: VM, args: Seq[Value]) = args match
    case f +: args => vm.appNever(f, args: _*)
    case _         => throw EvaluationError(s"$name takes at least one argument")

object BuiltinStr extends CommonBuiltinImpl:
  def name = "str"
  def run(vm: VM, args: Seq[Value]) =
    val Rest(nums) = take[Rest[Double]](args)
    val bytes = nums map { num =>
      if num < 0 || 255 < num then throw EvaluationError("Each byte of string must be inside the range 0-255")
      num.toByte
    }
    vm.push(Sexp.Str(bytes.toArray))

object BuiltinStrRef extends CommonBuiltinImpl:
  def name = "str-ref"
  def run(vm: VM, args: Seq[Value]) =
    val (s, index) = take[(Sexp.Str, Int)](args)
    vm.push(if index < 0 || s.data.length <= index then Sexp.Nil else Sexp.Num(s.data(index).toInt & 0xff))

object BuiltinStrBytesize extends CommonBuiltinImpl:
  def name = "str-bytesize"
  def run(vm: VM, args: Seq[Value]) =
    val s = take[Sexp.Str](args)
    vm.push(Sexp.Num(s.data.length))

object BuiltinStrConcat extends CommonBuiltinImpl:
  def name = "str-concat"
  def run(vm: VM, args: Seq[Value]) =
    val Rest(ss) = take[Rest[Sexp.Str]](args)
    val buf = ByteBuffer.allocate(ss.map(_.data.length).sum)
    for s <- ss do buf.put(s.data)
    vm.push(Sexp.Str(buf.array))

object BuiltinSubstr extends CommonBuiltinImpl:
  def name = "substr"
  def run(vm: VM, args: Seq[Value]) =
    val (s, index, size) = take[(Sexp.Str, Int, Int)](args)
    if index < 0 || s.data.length < index + size then throw EvaluationError("Index out of range")
    vm.push(Sexp.Str(s.data.slice(index, index + size)))

object BuiltinSymToStr extends CommonBuiltinImpl:
  def name = "sym->str"
  def run(vm: VM, args: Seq[Value]) =
    val arg = take[Sexp.Sym](args)
    vm.push(Sexp.Str.fromString(arg.data))

object BuiltinNumToStr extends CommonBuiltinImpl:
  def name = "num->str"
  def run(vm: VM, args: Seq[Value]) =
    val n = take[Sexp.Num](args)
    vm.push(Sexp.Str.fromString(n.inspect))

object BuiltinStrToNum extends CommonBuiltinImpl:
  def name = "str->num"
  def run(vm: VM, args: Seq[Value]) =
    val s = take[String](args)
    vm.push(TryEval(s.toDouble).fold(_ => Sexp.Nil, Sexp.Num.apply))

object BuiltinVec extends CommonBuiltinImpl:
  def name = "vec"
  def run(vm: VM, args: Seq[Value]) =
    vm.push(Sexp.Pure(Native.Vec(args.toArray)))

object BuiltinVecMake extends CommonBuiltinImpl:
  def name = "vec-make"
  def run(vm: VM, args: Seq[Value]) =
    val (length, init) = take[(Int, Value)](args)
    val array = new Array[Value](length)
    for i <- array.indices do array(i) = init
    vm.push(Sexp.Pure(Native.Vec(array)))

object BuiltinVecRef extends CommonBuiltinImpl:
  def name = "vec-ref"
  def run(vm: VM, args: Seq[Value]) =
    val (vec, index) = take[(Native.Vec, Int)](args)
    vm.push(if index < 0 || vec.payload.size <= index then Sexp.Nil else vec.payload(index))

object BuiltinVecLength extends CommonBuiltinImpl:
  def name = "vec-length"
  def run(vm: VM, args: Seq[Value]) =
    val vec = take[Native.Vec](args)
    vm.push(Sexp.Num(vec.payload.length))

object BuiltinVecSet extends CommonBuiltinImpl:
  def name = "vec-set!"
  def run(vm: VM, args: Seq[Value]) =
    val (vec, index, item) = take[(Native.Vec, Int, Value)](args)
    if index < 0 || vec.payload.length <= index then throw EvaluationError("Index out of range")
    vec.payload(index) = item
    vm.push(Sexp.Nil)

object BuiltinVecCopy extends CommonBuiltinImpl:
  def name = "vec-copy!"
  def run(vm: VM, args: Seq[Value]) =
    val (destVec, destStart, srcVec, srcStart, length) = take[(Native.Vec, Int, Native.Vec, Int, Int)](args)
    if 0 <= srcStart && srcStart + length <= srcVec.payload.length && 0 <= destStart && destStart + length <= destVec.payload.length then
      srcVec.payload.drop(srcStart).copyToArray(destVec.payload, destStart, length)
      vm.push(Sexp.Nil)
    else throw EvaluationError("Index out of range")

object BuiltinOpen extends CommonBuiltinImpl:
  def name = "open"
  def run(vm: VM, args: Seq[Value]) =
    val (filepath, mode) = take[(String, String)](args)
    TryEval {
      mode match
        case "r" =>
          val path = FileSystems.getDefault().getPath(filepath)
          val stream = Files.newInputStream(path)
          Sexp.Pure(Native.Port(InputStreamPortImpl(stream)))
        case "w" =>
          val path = FileSystems.getDefault().getPath(filepath)
          val stream = Files.newOutputStream(path)
          Sexp.Pure(Native.Port(OutputStreamPortImpl(stream)))
        case _ =>
          throw EvaluationError("Unsupported mode for open: " + mode)
    } match
      case Right(a) => vm.push(Sexp.Cons(Sexp.True, a))
      case Left(e)  => vm.push(Sexp.Cons(Sexp.False, Sexp.Str.fromString(e)))

object BuiltinClose extends CommonBuiltinImpl:
  def name = "close"
  def run(vm: VM, args: Seq[Value]) =
    val port = take[Native.Port](args)
    port.impl.close() match
      case Right(_) => vm.push(Sexp.Cons(Sexp.True, Sexp.Nil))
      case Left(e)  => vm.push(Sexp.Cons(Sexp.False, Sexp.Str.fromString(e)))

class BuiltinPortImpl(val name: String, val portImpl: PortImpl) extends CommonBuiltinImpl:
  def run(vm: VM, args: Seq[Value]) =
    take[Unit](args)
    vm.push(Sexp.Pure(Native.Port(portImpl)))

object BuiltinStdin extends BuiltinPortImpl("stdin", InputStreamPortImpl(System.in))
object BuiltinStdout extends BuiltinPortImpl("stdout", OutputStreamPortImpl(System.out))
object BuiltinStderr extends BuiltinPortImpl("stderr", OutputStreamPortImpl(System.err))

private transparent trait ReadBuiltinOps:
  def enforceIn(p: Native.Port): PortIn = p.impl.in match
    case Some(p) => p
    case None    => throw EvaluationError("port is not available for reading")

  def readResult[A](r: Either[String, Option[A]])(convert: A => Value): Value =
    r match
      case Right(Some(v)) => Sexp.Cons(Sexp.True, convert(v))
      case Right(None)    => Sexp.Cons(Sexp.True, Sexp.Sym("eof"))
      case Left(e)        => Sexp.Cons(Sexp.False, Sexp.Str.fromString(e))

object BuiltinReadByte extends CommonBuiltinImpl, ReadBuiltinOps:
  def name = "read-byte"
  def run(vm: VM, args: Seq[Value]) =
    val port = take[Native.Port](args)
    vm.push(readResult(enforceIn(port).readByte()) { v => Sexp.Num(v.toInt & 0xff) })

object BuiltinReadStr extends CommonBuiltinImpl, ReadBuiltinOps:
  def name = "read-str"
  def run(vm: VM, args: Seq[Value]) =
    val (size, port) = take[(Int, Native.Port)](args)
    vm.push(readResult(enforceIn(port).readBytes(size))(Sexp.Str.apply))

object BuiltinReadLine extends CommonBuiltinImpl, ReadBuiltinOps:
  def name = "read-line"
  def run(vm: VM, args: Seq[Value]) =
    val port = take[Native.Port](args)
    vm.push(readResult(enforceIn(port).readLine())(Sexp.Str.apply))

private transparent trait WriteBuiltinOps:
  def enforceOut(p: Native.Port): PortOut = p.impl.out match
    case Some(p) => p
    case None    => throw EvaluationError("port is not available for writing")

  def writeResult(r: Either[String, Int]): Value =
    r match
      case Right(i) => Sexp.Cons(Sexp.True, if i == 0 then Sexp.Nil else Sexp.Num(i))
      case Left(e)  => Sexp.Cons(Sexp.False, Sexp.Str.fromString(e))

object BuiltinWriteByte extends CommonBuiltinImpl, WriteBuiltinOps:
  def name = "write-byte"
  def run(vm: VM, args: Seq[Value]) =
    val (b, port) = take[(Int, Native.Port)](args)
    vm.push(writeResult(enforceOut(port).writeByte(b.toByte)))

object BuiltinWriteStr extends CommonBuiltinImpl, WriteBuiltinOps:
  def name = "write-str"
  def run(vm: VM, args: Seq[Value]) =
    val (s, port) = take[(Sexp.Str, Native.Port)](args)
    vm.push(writeResult(enforceOut(port).writeBytes(s.data)))

object BuiltinWriteLine extends CommonBuiltinImpl, WriteBuiltinOps:
  def name = "write-line"
  def run(vm: VM, args: Seq[Value]) =
    val (s, port) = take[(Sexp.Str, Native.Port)](args)
    vm.push(writeResult(enforceOut(port).writeLine(s.data)))

object BuiltinFlush extends CommonBuiltinImpl, WriteBuiltinOps:
  def name = "flush"
  def run(vm: VM, args: Seq[Value]) =
    val port = take[Native.Port](args)
    vm.push(writeResult(enforceOut(port).flush()))

final class BuiltinArgs(val envArgs: Seq[String]) extends CommonBuiltinImpl:
  def name = "args"
  def run(vm: VM, args: Seq[Value]) =
    take[Unit](args)
    vm.push(Sexp.List(envArgs.map(Sexp.Str.fromString): _*))

object BuiltinEval extends CommonBuiltinImpl:
  def name = "eval"
  def run(vm: VM, args: Seq[Value]) =
    val e = take[Value](args)
    vm.context(_.eval(e)) match
      case Right(v) => vm.push(Sexp.Cons(Sexp.True, v))
      case Left(e)  => vm.push(Sexp.Cons(Sexp.False, Sexp.Str.fromString(e)))

trait BuiltinMacroExpandImpl(recurse: Boolean) extends CommonBuiltinImpl:
  def run(vm: VM, args: Seq[Value]) =
    val e = take[Value](args)
    vm.context(_.macroExpand(recurse, e)) match
      case Right(v) => vm.push(Sexp.Cons(Sexp.True, v))
      case Left(e)  => vm.push(Sexp.Cons(Sexp.False, Sexp.Str.fromString(e)))

object BuiltinMacroExpand extends BuiltinMacroExpandImpl(true):
  def name = "macroexpand"

object BuiltinMacroExpandOnce extends BuiltinMacroExpandImpl(false):
  def name = "macroexpand-1"

class InputStreamPortImpl(s: InputStream) extends PortImpl:
  val stream = BufferedInputStream(s)

  def in = Some(portIn)
  def out = None
  def close() = TryEval(stream.close())

  def portIn = new PortIn:
    def readByte() = TryEval {
      val ch = stream.read()
      if ch != -1 then Some(ch.toByte) else None
    }

    def readBytes(size: Int) = TryEval {
      val bytes = new Array[Byte](size)
      val numRead = stream.read(bytes)
      if numRead != -1 then Some(bytes.slice(0, numRead)) else None
    }

    def readLine() = TryEval {
      val line = scala.collection.mutable.ListBuffer.empty[Array[Byte]]
      var terminate = false
      while !terminate do
        val bytes = new Array[Byte](256)
        stream.mark(256)
        val numRead = stream.read(bytes)
        val newline = (0 until numRead).find { i => bytes(i) == 0x0a }
        if numRead == -1 then terminate = true // terminated by EOF
        else
          newline match
            case Some(index) =>
              line += bytes.slice(0, index) // excluding newline
              stream.reset()
              stream.skip(index + 1)
              terminate = true // terminated by newline
            case None =>
              line += bytes.slice(0, numRead)

      if line.isEmpty then None
      else
        val buf = ByteBuffer.allocate(line.map(_.length).sum)
        for chunk <- line do buf.put(chunk)
        Some(buf.array)
    }

class OutputStreamPortImpl(s: OutputStream) extends PortImpl:
  val stream = BufferedOutputStream(s)

  def in = None
  def out = Some(portOut)
  def close() = TryEval {
    stream.flush()
    stream.close()
  }

  def portOut = new PortOut:
    def writeByte(byte: Byte) = TryEval {
      stream.write(byte.toInt)
      1
    }

    def writeBytes(bytes: Array[Byte]) = TryEval {
      stream.write(bytes)
      bytes.length
    }

    def writeLine(line: Array[Byte]) = TryEval {
      stream.write(line)
      stream.write(0x0a)
      stream.flush()
      line.length + 1
    }

    def flush() = TryEval {
      stream.flush()
      0
    }
