package scalisp

private[scalisp]
case class Pattern(val fixed: Seq[String], val rest: Option[String]) {
  def bind[A](env: Env[Sexp[A]], args: Sexp[A]*): Unit = {
    val paramc = fixed.size
    val argc = args.size
    if (argc < paramc) {
      val prefix = rest map (_ => "at least ") getOrElse ""
      throw new EvaluationError(s"This function takes $prefix$paramc arguments")
    }
    for ((p, a) <- fixed zip args) env.define(p, a)
    for (r <- rest) env.define(r, List(args.drop(paramc): _*))
  }

  override def toString(): String =
    ListLike(fixed map Sym.apply, rest map Sym.apply getOrElse Nil).inspect
}

private[scalisp]
object Pattern {
  def apply(pat: Sexp[Any]): Pattern = {
    val Some((init, last)) = ListLike.unapply(pat)
    val fixed = init map {
      case Sym(sym) => sym
      case a => throw new EvaluationError("Unsupported pattern: " + a.inspect)
    }
    val rest = last match {
      case Sym(sym) => Some(sym)
      case Nil => None
      case s => throw new EvaluationError("Unsupported pattern: " + s.inspect)
    }
    Pattern(fixed, rest)
  }
}
