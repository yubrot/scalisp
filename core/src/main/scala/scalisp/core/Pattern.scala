package scalisp.core

case class Pattern(fixed: Seq[String], rest: Option[String]):
  def bind[A <: Inspect](env: Env[Sexp[A]], args: Sexp[A]*): Unit =
    val paramc = fixed.size
    val argc = args.size
    if argc < paramc || (rest.isEmpty && argc > paramc) then
      val prefix = rest map (_ => "at least ") getOrElse ""
      throw EvaluationError(s"This function takes $prefix$paramc arguments")
    for (p, a) <- fixed zip args do env.define(p, a)
    for r <- rest do env.define(r, Sexp.List(args.drop(paramc): _*))

  override def toString(): String =
    Sexp.ListLike(fixed map Sexp.Sym.apply, rest map Sexp.Sym.apply getOrElse Sexp.Nil).inspect

object Pattern:
  def apply(pat: Sexp[? <: Inspect]): Pattern =
    val Some((init, last)) = Sexp.ListLike.unapply(pat)
    val fixed = init map {
      case Sexp.Sym(sym) => sym
      case a             => throw EvaluationError("Unsupported pattern: " + a.inspect)
    }
    val rest = last match
      case Sexp.Sym(sym) => Some(sym)
      case Sexp.Nil      => None
      case s             => throw EvaluationError("Unsupported pattern: " + s.inspect)
    Pattern(fixed, rest)
