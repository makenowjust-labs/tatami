package codes.quine.labo
package tatami

import cats.Applicative
import cats.Monad
import cats.kernel.Eq
import cats.syntax.functor._

import hariko.Cogen
import hariko.Gen
import hariko.Param
import Request._

object TestImplicits {
  implicit def sourceGen[A: Gen]: Gen[Source[A]] = Gen[List[A]].map(Source.from(_))

  implicit def requestGen[M[_], A, B](implicit A: Cogen[A], MB: Gen[M[B]]): Gen[Request[M, A, B]] =
    Gen.frequency(2 -> Gen[A => M[B]].map(Pull(_)), 1 -> MB.map(Done(_)))

  implicit def foldWithStateGen[M[_], A, B, S0](implicit
      MB: Gen[M[B]],
      S0: Cogen[S0],
      MS0: Gen[M[S0]],
      MReq: Gen[M[Request[M, A, S0]]]
  ): Gen[Fold[M, A, B] { type S = S0 }] =
    Gen.map3(Gen[M[S0]], Gen[S0 => M[Request[M, A, S0]]], Gen[S0 => M[B]]) {
      case (i, f, g) =>
        new Fold[M, A, B] {
          type S = S0
          def start: M[S0] = i
          def step(s: S0): M[Request[M, A, S0]] = f(s)
          def end(s: S0): M[B] = g(s)
          override def toString: String = s"Fold($i, $f, $g)"
        }
    }

  implicit def foldGen[M[_], A, B](implicit
      MB: Gen[M[B]],
      MInt: Gen[M[Int]],
      MReq: Gen[M[Request[M, A, Int]]]
  ): Gen[Fold[M, A, B]] =
    foldWithStateGen[M, A, B, Int].map(_.asInstanceOf[Fold[M, A, B]])

  implicit def foldCogen[M[_], A, B](implicit A: Gen[A], M: Monad[M], MB: Cogen[M[B]]): Cogen[Fold[M, A, B]] =
    Cogen[List[A] => M[B]].imap(f =>
      new Fold[M, A, B] {
        val fold = Fold.toList[M, A].rmapM(f)
        type S = fold.S
        def start: M[S] = fold.start
        def step(s: S): M[Request[M, A, S]] = fold.step(s)
        def end(s: S): M[B] = fold.end(s)
        override def toString: String = s"Fold.toList.rmapM($f)"
      }: Fold[M, A, B]
    )(fold => xs => Fold.apply(xs, fold).map(_._1))

  implicit def transGen[M[_]: Applicative, A: Cogen, B: Gen]: Gen[Trans[M, A, B]] =
    Gen[A => Option[B]].map(f =>
      new Trans[M, A, B] {
        def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] = fbc.lmapFilter(f)
        override def toString(): String = s"Trans($f)"
      }
    )

  implicit def catsKernelEqInstanceForFunction1[A: Gen, B: Eq]: Eq[A => B] =
    Eq.by(f => Gen[A].samples(Param(42)).take(30).map(f))

  implicit def catsKernelEqInstanceForRequest[M[_], A, B](implicit A: Gen[A], MB: Eq[M[B]]): Eq[Request[M, A, B]] =
    Eq.by {
      case Pull(f)  => Left(f)
      case Done(ms) => Right(ms)
    }

  implicit def catsKernelEqInstanceForFold[M[_], A, B](implicit
      A: Gen[A],
      M: Monad[M],
      MB: Eq[M[(B, Source[A])]]
  ): Eq[Fold[M, A, B]] =
    Eq.by(fold => Gen[List[A]].samples(Param(42)).take(5).map(Fold.apply(_, fold)))

  implicit def catsKernelEqInstanceForTrans[M[_], A, B](implicit
      A: Gen[A],
      M: Monad[M],
      MInt: Gen[M[Int]],
      MReq: Gen[M[Request[M, B, Int]]],
      EqMB: Eq[M[(B, Source[A])]],
      GenMB: Gen[M[B]]
  ): Eq[Trans[M, A, B]] =
    Eq.by(t => Gen[Fold[M, B, B]].samples(Param(42)).take(5).map(t.trans(_)))
}
