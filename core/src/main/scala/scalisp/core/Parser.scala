package scalisp.core

import cats.parse.{Parser => P, Parser0 => P0}

object Parser:
  object Token:
    private val letter: Seq[Char] = ('A' to 'Z') ++ ('a' to 'z')
    private val digit: Seq[Char] = '0' to '9'
    private val special: Seq[Char] = "!$%&*+-./:<=>?@^_~"

    val number: P[Double] =
      val digits = P.charIn(digit).rep.void
      val frac = P.string(".") *> digits.?
      val exp = P.charIn("eE") *> P.charIn("-+").? *> digits
      (P.charIn("-+").?.with1 *> digits *> frac.? *> exp.?).string map (_.toDouble)

    val symbol: P[String] =
      val first = P.charIn(letter ++ special).void
      val rest = P.charIn(letter ++ digit ++ special).rep0.void
      (first *> rest).string.filter(_ != ".")

    val string: P[String] =
      val a = P.charsWhile(c => !"\\\"".contains(c)).void
      val escapeSequence = P.string("\\") *> P.charIn("\\tn\"").void
      val body = (a | escapeSequence).rep0.void
      (P.string("\"") *> body.string <* P.string("\"")) map Sexp.Str.unescape

  private val spaces: P[Unit] = P.charIn(" \t\n").rep.void
  private val comment: P[Unit] = (P.char(';') ~ P.charsWhile0(_ != '\n')).void
  private val amb: P0[Unit] = (spaces | comment).rep0.void

  private def just(s: String): P[Unit] = P.string(s) <* amb
  private def just[A](p: P[A]): P[A] = p <* amb

  type S = Sexp[Nothing]

  val s: P[S] = P.recursive[S] { recurse =>
    val s: P[S] = just(recurse)
    val listLikeInner: P0[S] = (s.rep ~ (just(".") *> s).?).? map {
      case Some(ss, t) => Sexp.ListLike(ss.toList, t getOrElse Sexp.Nil)
      case None        => Sexp.Nil
    }
    val listLike = (just("(") *> listLikeInner <* just(")")) | (just("[") *> listLikeInner <* just("]"))
    val quote = just("'") *> s map Sexp.Quote.apply
    val quasiquote = just("`") *> s map Sexp.Quasiquote.apply
    val unquoteSplicing = just(",@") *> s map Sexp.UnquoteSplicing.apply
    val unquote = just(",") *> s map Sexp.Unquote.apply
    val num = just(Token.number) map Sexp.Num.apply
    val sym = just(Token.symbol) map Sexp.Sym.apply
    val str = just(Token.string) map Sexp.Str.apply
    val t = just("#t") map (_ => Sexp.True)
    val f = just("#f") map (_ => Sexp.False)
    listLike | quote | quasiquote | unquoteSplicing | unquote | num.backtrack | sym.backtrack | str | t | f
  }

  val program: P0[Seq[S]] = amb *> s.rep0 <* P.end

  val unit: P0[Option[S]] =
    val some = s map Some.apply
    val none = P.end map (_ => None)
    amb *> (some | none)
