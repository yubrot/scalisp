package scalisp.core

trait Inspect:
  def inspect: String

enum Sexp[+A <: Inspect] extends Inspect:
  case Num(data: Double) extends Sexp[Nothing]
  case Sym(data: String) extends Sexp[Nothing]
  case Str(data: String) extends Sexp[Nothing]
  case True extends Sexp[Nothing]
  case False extends Sexp[Nothing]
  case Cons(car: Sexp[A], cdr: Sexp[A]) extends Sexp[A]
  case Nil extends Sexp[Nothing]
  case Pure(data: A) extends Sexp[A]

  def test: Boolean = this match
    case False => false
    case _     => true

  def inspect: String = this match
    // NOTE: Is there a way to omit `Sexp.`?
    case Sexp.Quote(s)           => "'" + s.inspect
    case Sexp.Quasiquote(s)      => "`" + s.inspect
    case Sexp.Unquote(s)         => "," + s.inspect
    case Sexp.UnquoteSplicing(s) => ",@" + s.inspect
    case Num(data)               => (if data % 1.0 != 0 then "%s" else "%.0f") format data
    case Sym(data)               => data
    case Str(data)               => "\"" + Str.escape(data) + "\""
    case True                    => "#t"
    case False                   => "#f"
    case Cons(car, cdr)          => "(" + inspectListLikeInner(car, cdr) + ")"
    case Nil                     => "()"
    case Pure(data)              => data.inspect

  private def inspectListLikeInner(car: Sexp[A], cdr: Sexp[A]): String = cdr match
    case Cons(cdar, cddr) => car.inspect + " " + inspectListLikeInner(cdar, cddr)
    case Nil              => car.inspect
    case cdr              => car.inspect + " . " + cdr.inspect

  override def toString(): String = this match
    case Str(data) => data
    case _         => super.toString()

object Sexp:
  object Bool:
    def apply(b: Boolean): Sexp[Nothing] = if b then True else False
    def unapply(s: Sexp[? <: Inspect]): Option[Boolean] = s match
      case True  => Some(true)
      case False => Some(false)
      case _     => None

  object Str:
    def escape(data: String): String = data.flatMap {
      case '\\' => "\\\\"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '"'  => "\\\""
      case c    => c.toString
    }

    def unescape(data: String): String =
      val buf = scala.collection.mutable.StringBuilder()
      var escape = false
      for c <- data do
        if escape then
          buf += (c match
            case '\\' => '\\'
            case 't'  => '\t'
            case 'n'  => '\n'
            case '"'  => '"'
            case _    => throw scalisp.core.InternalError("Unknown escape sequence: \\" + c)
          )
          escape = false
        else
          c match
            case '\\' => escape = true
            case c    => buf += c
      if escape then throw scalisp.core.InternalError("Unexpected terminal escape")
      buf.result

  object List:
    def apply[A <: Inspect](ss: Sexp[A]*): Sexp[A] = ListLike(ss, Nil)
    def unapplySeq[A <: Inspect](s: Sexp[A]): Option[Seq[Sexp[A]]] = s match
      case Nil                    => Some(Seq.empty)
      case Cons(a, List(bs @ _*)) => Some(a +: bs)
      case _                      => None

  object ListLike:
    def apply[A <: Inspect](ss: Seq[Sexp[A]], last: Sexp[A]): Sexp[A] = ss.foldRight(last)(Cons.apply)
    def unapply[A <: Inspect](s: Sexp[A]): Option[(Seq[Sexp[A]], Sexp[A])] = s match
      case Cons(a, ListLike(ss, s)) => Some((a +: ss, s))
      case s                        => Some((Seq.empty, s))

  class SyntaxSugar(private val name: String):
    def apply[A <: Inspect](s: Sexp[A]): Sexp[A] = List(Sym(name), s)
    def unapply[A <: Inspect](s: Sexp[A]): Option[Sexp[A]] = s match
      case List(Sym(sym), s) if sym == name => Some(s)
      case _                                => None

  object Quote extends SyntaxSugar("quote")
  object Quasiquote extends SyntaxSugar("quasiquote")
  object Unquote extends SyntaxSugar("unquote")
  object UnquoteSplicing extends SyntaxSugar("unquote-splicing")
