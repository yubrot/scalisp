package scalisp

import java.io.{InputStream, BufferedInputStream, OutputStream, BufferedOutputStream}
import java.nio.ByteBuffer
import java.nio.file.{FileSystems, Files}

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
    val filepath = Str.decode(takeStr("filepath", p))
    val mode = Str.decode(takeStr("mode", m))

    TryEval {
      mode match {
        case "r" =>
          val path = FileSystems.getDefault().getPath(filepath)
          val stream = Files.newInputStream(path)
          Pure(Port(new InputStreamPortImpl(stream)))
        case "w" =>
          val path = FileSystems.getDefault().getPath(filepath)
          val stream = Files.newOutputStream(path)
          Pure(Port(new OutputStreamPortImpl(stream)))
        case _ =>
          throw new EvaluationError("Unsupported mode for open: " + mode)
      }
    } match {
      case Right(a) => vm.push(Cons(True, a))
      case Left(e) => vm.push(Cons(False, Str.fromString(e)))
    }
  }
}

object BuiltinStdin extends BuiltinPortImpl("stdin", new InputStreamPortImpl(System.in))
object BuiltinStdout extends BuiltinPortImpl("stdout", new OutputStreamPortImpl(System.out))
object BuiltinStderr extends BuiltinPortImpl("stderr", new OutputStreamPortImpl(System.err))

class InputStreamPortImpl(s: InputStream) extends PortImpl {
  val stream = new BufferedInputStream(s)

  def in = Some(portIn)
  def out = None
  def close() = TryEval(stream.close())

  def portIn = new PortIn {
    def readByte() = TryEval {
      val ch = stream.read()
      if (ch != -1) Some(ch.toByte) else None
    }

    def readBytes(size: Int) = TryEval {
      val bytes = new Array[Byte](size)
      val numRead = stream.read(bytes)
      if (numRead != -1) Some(bytes.slice(0, numRead)) else None
    }

    def readLine() = TryEval {
      val line = scala.collection.mutable.ListBuffer.empty[Array[Byte]]
      var terminate = false
      while (!terminate) {
        val bytes = new Array[Byte](256)
        stream.mark(256)
        val numRead = stream.read(bytes)
        val newline = (0 until numRead).find { i => bytes(i) == 0x0a }
        if (numRead == -1) {
          terminate = true // terminated by EOF
        } else newline match {
          case Some(index) =>
            line += bytes.slice(0, index) // excluding newline
            stream.reset()
            stream.skip(index + 1)
            terminate = true // terminated by newline
          case None =>
            line += bytes.slice(0, numRead)
        }
      }
      if (line.isEmpty) None else {
        val buf = ByteBuffer.allocate(line.map(_.length).sum)
        for (chunk <- line) buf.put(chunk)
        Some(buf.array)
      }
    }
  }
}

class OutputStreamPortImpl(s: OutputStream) extends PortImpl {
  val stream = new BufferedOutputStream(s)

  def in = None
  def out = Some(portOut)
  def close() = TryEval {
    stream.flush()
    stream.close()
  }

  def portOut = new PortOut {
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
  }
}
