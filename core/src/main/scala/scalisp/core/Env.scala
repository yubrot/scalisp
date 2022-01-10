package scalisp.core

import scala.collection.mutable.Map

class UndefinedVariable(val name: String) extends Exception

class Env[A](private val parent: Option[Env[A]] = None):
  private val current = Map.empty[String, A]

  def define(k: String, v: A): Unit = current(k) = v

  def set(k: String, v: A): Unit =
    var env = this
    while !env.current.isDefinedAt(k) do
      env.parent match
        case Some(e) => env = e
        case None    => throw UndefinedVariable(k)
    env.current(k) = v

  def find(k: String): Option[A] = current.get(k) orElse parent.flatMap(_.find(k))

  def get(k: String): A = find(k) getOrElse (throw UndefinedVariable(k))

  def refer(k: Sexp[? <: Inspect]): Option[A] = k match
    case Sexp.Sym(sym) => find(sym)
    case _             => None
