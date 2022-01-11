package scalisp.core

import cats.implicits.*

trait MatchArg[T]:
  def signature: String
  def matchArg(arg: Value): Option[T]

given MatchArg[Value] with
  def signature: String = "value"
  def matchArg(arg: Value): Option[Value] = Some(arg)

class MatchArgByCast[T](sig: String)(cast: PartialFunction[Value, T]) extends MatchArg[T]:
  def signature: String = sig
  def matchArg(arg: Value): Option[T] = if cast isDefinedAt arg then Some(cast(arg)) else None

given MatchArg[Sexp.Num] = MatchArgByCast("num") { case num @ Sexp.Num(_) => num }
given MatchArg[Double] = MatchArgByCast("num") { case Sexp.Num(num) => num }
given MatchArg[Int] = MatchArgByCast("num") { case Sexp.Num(num) => num.toInt }
given MatchArg[Sexp.Sym] = MatchArgByCast("sym") { case sym @ Sexp.Sym(_) => sym }
given MatchArg[Sexp.Str] = MatchArgByCast("str") { case str @ Sexp.Str(_) => str }
given MatchArg[String] = MatchArgByCast("str") { case str @ Sexp.Str(_) => str.toString }
given MatchArg[Sexp.Cons[Native]] = MatchArgByCast("cons") { case cons @ Sexp.Cons(_, _) => cons }
given MatchArg[Native.Port] = MatchArgByCast("port") { case Sexp.Pure(port @ Native.Port(_)) => port }
given MatchArg[Native.Vec] = MatchArgByCast("vec") { case Sexp.Pure(vec @ Native.Vec(_)) => vec }

given [T: MatchArg]: MatchArg[Seq[T]] with
  def signature: String =
    Seq(summon[MatchArg[T]].signature, "...").mkString("(", " ", ")")
  def matchArg(arg: Value): Option[Seq[T]] = arg match
    case Sexp.List(args @ _*) => args.traverse(summon[MatchArg[T]].matchArg)
    case _                    => None

given [L: MatchArg, R: MatchArg]: MatchArg[Either[L, R]] with
  def signature: String =
    s"<${summon[MatchArg[L]].signature} or ${summon[MatchArg[R]].signature}>"
  def matchArg(arg: Value): Option[Either[L, R]] =
    summon[MatchArg[L]].matchArg(arg).map(Left.apply) orElse
      summon[MatchArg[R]].matchArg(arg).map(Right.apply)

trait MatchArgs[T]:
  def signature: Seq[String]
  def matchArgs(args: Seq[Value]): Option[T]

given [T: MatchArg]: MatchArgs[T] with
  def signature: Seq[String] =
    Seq(summon[MatchArg[T]].signature)
  def matchArgs(args: Seq[Value]): Option[T] = args match
    case Seq(a) => summon[MatchArg[T]].matchArg(a)
    case _      => None

given matchZeroArguments: MatchArgs[Unit] with
  def signature: Seq[String] =
    Seq()
  def matchArgs(args: Seq[Value]): Option[Unit] = args match
    case Seq() => Some({})
    case _     => None

given matchLastArgument[T: MatchArgs]: MatchArgs[T *: EmptyTuple] with
  def signature: Seq[String] =
    summon[MatchArgs[T]].signature
  def matchArgs(args: Seq[Value]): Option[T *: EmptyTuple] =
    summon[MatchArgs[T]].matchArgs(args) map { _ *: EmptyTuple }

given matchTwoOrMoreArguments[H: MatchArg, T <: NonEmptyTuple: MatchArgs]: MatchArgs[H *: T] with
  def signature: Seq[String] =
    summon[MatchArg[H]].signature +: summon[MatchArgs[T]].signature
  def matchArgs(args: Seq[Value]): Option[H *: T] = args match
    case h +: t =>
      for
        h1 <- summon[MatchArg[H]].matchArg(h)
        t1 <- summon[MatchArgs[T]].matchArgs(t)
      yield h1 *: t1
    case _ => None

case class Rest[T](args: Seq[T])

given matchRestArguments[T: MatchArg]: MatchArgs[Rest[T]] with
  def signature: Seq[String] =
    Seq(summon[MatchArg[T]].signature, "...")
  def matchArgs(args: Seq[Value]): Option[Rest[T]] =
    args.traverse(summon[MatchArg[T]].matchArg) map Rest.apply
