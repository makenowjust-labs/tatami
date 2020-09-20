package codes.quine.labo.tatami

import cats.Alternative
import cats.Applicative
import cats.Eval
import cats.Foldable
import cats.Monad
import cats.Traverse
import cats.instances.list.catsKernelStdEqForList
import cats.kernel.Eq

final case class Source[A](eval: Eval[Option[(A, Source[A])]]) {
  def uncons: Option[(A, Source[A])] = eval.value
}

object Source extends SourceInstances0 {
  def empty[A]: Source[A] = Source(Eval.now(None))

  def cons[A](a: A, src: Source[A]): Source[A] = Source(Eval.now(Some((a, src))))

  def from[F[_], A](fa: F[A])(implicit F: Foldable[F]): Source[A] = {
    val eval = F.foldRight(fa, Eval.now(Option.empty[(A, Source[A])]))((a, las) => Eval.later(Some((a, Source(las)))))
    Source(eval)
  }
}

private[tatami] trait SourceInstances0 {
  implicit def catsKernelEqInstanceForSource[A: Eq]: Eq[Source[A]] =
    Eq.by[Source[A], List[A]](Foldable[Source].toList(_))

  implicit val catsInstancesForSource: Alternative[Source] with Monad[Source] with Traverse[Source] =
    new Alternative[Source] with Monad[Source] with Traverse[Source] {
      def pure[A](a: A): Source[A] = Source(Eval.now(Some((a, Source.empty[A]))))
      def empty[A]: Source[A] = Source.empty[A]
      def combineK[A](x: Source[A], y: Source[A]): Source[A] = {
        val eval = foldRight(x, y.eval)((a, las) => Eval.later(Some((a, Source(las)))))
        Source(eval)
      }
      def flatMap[A, B](fa: Source[A])(f: A => Source[B]): Source[B] =
        Source(foldRight(fa, Eval.now(Option.empty[(B, Source[B])])) { (a, lbs) =>
          Eval.defer(foldRight(f(a), lbs)((b, lbs) => Eval.later(Some((b, Source(lbs))))))
        })
      def tailRecM[A, B](a: A)(f: A => Source[Either[A, B]]): Source[B] =
        flatMap(f(a)) {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => pure(b)
        }
      def traverse[G[_], A, B](src: Source[A])(f: A => G[B])(implicit G: Applicative[G]): G[Source[B]] =
        foldRight(src, Eval.now(G.pure(Source.empty[B]))) { (a, lgsrc) =>
          G.map2Eval(f(a), lgsrc)(Source.cons(_, _))
        }.value
      def foldLeft[A, B](src: Source[A], b: B)(f: (B, A) => B): B =
        Monad[Eval]
          .tailRecM((src, b)) { case (src0, b) =>
            src0.eval.map {
              case None            => Right(b)
              case Some((a, src1)) => Left((src1, f(b, a)))
            }
          }
          .value
      def foldRight[A, B](src: Source[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        src.eval.flatMap {
          case None            => lb
          case Some((a, tail)) => Eval.defer(f(a, Eval.defer(foldRight(tail, lb)(f))))
        }
    }
}
