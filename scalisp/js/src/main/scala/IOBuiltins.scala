package scalisp

object IOBuiltins extends Builtins {
  def register(context: Context): Unit = {
    put(context, BuiltinOpen)
    put(context, BuiltinStdin)
    put(context, BuiltinStdout)
    put(context, BuiltinStderr)
  }
}

object BuiltinOpen extends NamedBuiltinImpl {
  def name = "open"
  def run(vm: VM, args: Seq[Value]) = {
    val (p, m) = takeTwo(args)
    Str.decode(takeStr("filepath", p))
    Str.decode(takeStr("mode", m))
    vm.push(Cons(False, Str.fromString("open is unsupported")))
  }
}

object BuiltinStdin extends BuiltinPortImpl("stdin", new EmptyPortImpl)
object BuiltinStdout extends BuiltinPortImpl("stdout", new DelegatePortImpl(Scalisp.out _))
object BuiltinStderr extends BuiltinPortImpl("stderr", new DelegatePortImpl(Scalisp.err _))

class EmptyPortImpl extends PortImpl {
  def in = None
  def out = None
  def close() = Right(())
}

class DelegatePortImpl(val handler: Array[Byte] => Unit) extends PortImpl {
  def in = None
  def out = Some(portOut)
  def close() = Right(())

  def portOut = new PortOut {
    def writeByte(byte: Byte) = {
      handler(Array(byte))
      Right(1)
    }

    def writeBytes(bytes: Array[Byte]) = {
      handler(bytes)
      Right(bytes.length)
    }

    def writeLine(line: Array[Byte]) = {
      writeBytes(line)
      writeByte(0x0a)
      Right(line.length + 1)
    }

    def flush() = Right(0)
  }
}
