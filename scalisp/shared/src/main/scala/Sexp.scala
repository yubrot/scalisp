package scalisp

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

trait Inspect {
  def inspect(): String
}

abstract sealed class Sexp[+A] extends Inspect {
  def test(): Boolean = true
  def Cons[B >: A](cdr: Sexp[B]): Sexp[B] = scalisp.Cons(this, cdr)
}

case class Num(data: Double) extends Sexp[Nothing] {
  def inspect(): String = (if (data % 1.0 != 0) "%s" else "%.0f") format data
}

case class Sym(data: String) extends Sexp[Nothing] {
  def inspect(): String = data
}

case class Str(data: Array[Byte]) extends Sexp[Nothing] {
  def inspect(): String = "\"" + Str.escape(toString) + "\""

  override def toString(): String = {
    val buf = ByteBuffer.wrap(data)
    StandardCharsets.UTF_8.decode(buf).toString()
  }
}

abstract case class Bool(data: Boolean) extends Sexp[Nothing] {
  override def test(): Boolean = data
  def inspect(): String = if (data) "#t" else "#f"
}

case class Cons[A](car: Sexp[A], cdr: Sexp[A]) extends Sexp[A] {
  def inspect(): String = this match {
    case Quote(s) => "'" + s.inspect
    case Quasiquote(s) => "`" + s.inspect
    case Unquote(s) => "," + s.inspect
    case UnquoteSplicing(s) => ",@" + s.inspect
    case _ => "(" + inspectInner + ")"
  }

  def inspectInner(): String = cdr match {
    case cons@(_ Cons _) => car.inspect + " " + cons.inspectInner
    case Nil => car.inspect
    case cdr => car.inspect + " . " + cdr.inspect
  }
}

case class Pure[A <: Inspect](data: A) extends Sexp[A] {
  def inspect(): String = data.inspect
}

object Str {
  def apply(str: String): Str = {
    Str(str.getBytes(StandardCharsets.UTF_8))
  }

  def escape(data: String): String = {
    data.replaceAll("\\\\", "\\\\\\\\")
        .replaceAll("\t", "\\\\t")
        .replaceAll("\n", "\\\\n")
        .replaceAll("\"", "\\\\\"")
  }

  def unescape(data: String): String = {
    data.replaceAll("\\\\t", "\t")
        .replaceAll("\\\\n", "\n")
        .replaceAll("\\\\\"", "\"")
        .replaceAll("\\\\\\\\", "\\\\")
  }
}

object Nil extends Sexp[Nothing] {
  def inspect(): String = "()"
}

object True extends Bool(true)

object False extends Bool(false)

object Bool {
  def apply(b: Boolean): Bool = if (b) True else False
}

object List {
  def apply[A](ss: Sexp[A]*): Sexp[A] = ListLike(ss, Nil)
  def unapplySeq[A](s: Sexp[A]): Option[Seq[Sexp[A]]] = s match {
    case Nil => Some(Seq.empty)
    case a Cons List(bs@ _*) => Some(a +: bs)
    case _ => None
  }
}

object ListLike {
  def apply[A](ss: Seq[Sexp[A]], last: Sexp[A]): Sexp[A] = ss.foldRight(last)(Cons.apply _)
  def unapply[A](s: Sexp[A]): Option[(Seq[Sexp[A]], Sexp[A])] = s match {
    case a Cons ListLike(ss, s) => Some((a +: ss, s))
    case s => Some((Seq.empty, s))
  }
}

private[scalisp]
class SyntaxSugar(private val name: String) {
  def apply[A](s: Sexp[A]): Sexp[A] = List(Sym(name), s)
  def unapply[A](s: Sexp[A]): Option[Sexp[A]] = s match {
    case List(Sym(sym), s) if sym == name => Some(s)
    case _ => None
  }
}

object Quote extends SyntaxSugar("quote")
object Quasiquote extends SyntaxSugar("quasiquote")
object Unquote extends SyntaxSugar("unquote")
object UnquoteSplicing extends SyntaxSugar("unquote-splicing")
