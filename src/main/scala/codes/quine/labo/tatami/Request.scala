package codes.quine.labo.tatami

import cats.FlatMap
import cats.Functor
import cats.syntax.flatMap._
import cats.syntax.functor._

import Request._

sealed abstract class Request[M[_], A, B] extends Serializable with Product { req =>
  def lmap[Z](f: Z => A): Request[M, Z, B] =
    req match {
      case Pull(k)  => Pull((z: Z) => k(f(z)))
      case Done(mb) => Done(mb)
    }

  def lmapM[Z](f: Z => M[A])(implicit M: FlatMap[M]): Request[M, Z, B] =
    req match {
      case Pull(k)  => Pull((z: Z) => f(z).flatMap(k))
      case Done(mb) => Done(mb)
    }

  def rmap[C](f: B => C)(implicit M: Functor[M]): Request[M, A, C] =
    req match {
      case Pull(k)  => Pull((a: A) => k(a).map(f))
      case Done(mb) => Done(mb.map(f))
    }
}

object Request {
  final case class Pull[M[_], A, B](cont: A => M[B]) extends Request[M, A, B]
  final case class Done[M[_], A, B](value: M[B]) extends Request[M, A, B]
}
