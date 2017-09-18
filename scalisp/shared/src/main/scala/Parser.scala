package scalisp

object Parser {
  private val white = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._

    val spaces = CharsWhile(" \t\n".contains(_))
    val comment = ";" ~ CharsWhile(_ != '\n')
    NoTrace((spaces | comment).rep)
  }

  import fastparse.noApi._
  import white._

  type S = Sexp[Nothing]

  val program: Parser[Seq[S]] = P(Pass ~ s.rep ~ End)

  val line: Parser[Option[S]] = {
    val some = P(s) map Some.apply
    val none = P(End) map (_ => None)
    P(Pass ~ (some | none))
  }

  private val s: Parser[S] = {
    val cons = P("(" ~/ sInner ~ ")" | "[" ~/ sInner ~ "]")
    val num  = P(number) map Num.apply
    val sym  = P(symbol) map Sym.apply
    val str  = P(string) map Str.fromString
    val t    = P("#t") map (_ => True)
    val f    = P("#f") map (_ => False)
    P(cons | sQuoted | num | sym | str | t | f)
  }

  private val sQuoted: Parser[S] = {
    val quote           = ("'" ~/ s) map Quote.apply
    val quasiquote      = ("`" ~/ s) map Quasiquote.apply
    val unquoteSplicing = (",@" ~/ s) map UnquoteSplicing.apply
    val unquote         = ("," ~/ s) map Unquote.apply
    P(quote | quasiquote | unquoteSplicing | unquote)
  }

  private val sInner: Parser[S] = {
    val oneOrMore = (s.rep(1) ~/ ("." ~/ s).?) map { case (ss, t) => ListLike(ss, t getOrElse Nil) }
    val zero = Pass map (_ => Nil)
    P(oneOrMore | zero)
  }

  val letter = ('A' to 'Z') ++ ('a' to 'z')
  val digit = '0' to '9'
  val special = "!$%&*+-./:<=>?@^_~"

  val number: Parser[Double] = {
    val digits = CharsWhile(digit.contains(_))
    val frac = "." ~~ digits.?
    val exp = CharIn("eE").~/ ~~ CharIn("-+").? ~~ digits
    P(CharIn("-+").? ~~ digits ~~ frac.? ~~ exp.?).! map (_.toDouble)
  }

  val symbol: Parser[String] = {
    val first = letter ++ special
    val rest = letter ++ digit ++ special
    P(CharIn(first) ~~ CharsWhile(rest.contains(_), 0)).!.filter(_ != ".")
  }

  val string: Parser[String] = {
    val a = CharsWhile(!"\\\"".contains(_))
    val escapeSequence = "\\".~/ ~~ CharIn("\\tn\"")
    val text = (a | escapeSequence).repX
    P("\"".~/ ~~ text.! ~~ "\"") map Str.unescape
  }
}
