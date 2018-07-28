package object scalisp {
  type Value = Sexp[Native]
  type Code = Seq[Inst]
}
