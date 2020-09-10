package codes.quine.labo.tatami

import cats.Applicative
import cats.Comonad
import cats.Foldable
import cats.arrow.Compose
import cats.arrow.Profunctor

trait Fold[A, B] extends Serializable {
  type S

  def start: S
  def step(s: S, a: A): S
  def end(s: S): B

  def apply[F[_]: Foldable](fa: F[A]): B =
    end(Foldable[F].foldLeft(fa, start)(step))

  def lmap[Z](f: Z => A): Fold[Z, B] =
    Fold.of(start)((s, z) => step(s, f(z)), end(_))

  def rmap[C](f: B => C): Fold[A, C] =
    Fold.of(start)(step(_, _), s => f(end(s)))

  def dimap[Z, C](f: Z => A)(g: B => C): Fold[Z, C] =
    Fold.of(start)((s, z) => step(s, f(z)), s => g(end(s)))

  def andThen[C](g: Fold[B, C]): Fold[A, C] =
    Fold.of((start, g.start))(
      { case ((s0, sg), a) => val s1 = step(s0, a); (s1, g.step(sg, end(s1))) },
      { case (_, sg) => g.end(sg) }
    )
}

object Fold extends FoldInstances {
  type Aux[A, B, S0] = Fold[A, B] { type S = S0 }

  def of[A, B, S0](i: => S0)(f: (S0, A) => S0, g: S0 => B): Aux[A, B, S0] =
    new Fold[A, B] {
      type S = S0
      def start: S0 = i
      def step(s: S0, a: A): S0 = f(s, a)
      def end(s: S0): B = g(s)
    }

  def left[A, B, C](fc: Fold[A, C]): Fold[Either[A, B], C] =
    Fold.of(fc.start)((s, a) => a.fold(fc.step(s, _), _ => s), fc.end(_))

  def right[A, B, C](fc: Fold[B, C]): Fold[Either[A, B], C] =
    Fold.of(fc.start)((s, a) => a.fold(_ => s, fc.step(s, _)), fc.end(_))

  def forall[A](f: A => Boolean): Fold[A, Boolean] =
    of(true)(_ && f(_), identity)

  def exists[A](f: A => Boolean): Fold[A, Boolean] =
    of(false)(_ || f(_), identity)

  def maxOption[A: Ordering]: Fold[A, Option[A]] =
    of(Option.empty[A])((s, a) => Some(s.fold(a)(Ordering[A].max(_, a))), identity)

  def minOption[A: Ordering]: Fold[A, Option[A]] =
    of(Option.empty[A])((s, a) => Some(s.fold(a)(Ordering[A].min(_, a))), identity)

  def maxByOption[A, B: Ordering](f: A => B): Fold[A, Option[A]] =
    of(Option.empty[(A, B)])(
      {
        case (None, a) => Some((a, f(a)))
        case (Some((a0, b0)), a) =>
          val b = f(a)
          if (Ordering[B].lt(b0, b)) Some((a, b)) else Some((a0, b0))
      },
      _.map(_._1)
    )

  def minByOption[A, B: Ordering](f: A => B): Fold[A, Option[A]] =
    of(Option.empty[(A, B)])(
      {
        case (None, a) => Some((a, f(a)))
        case (Some((a0, b0)), a) =>
          val b = f(a)
          if (Ordering[B].gt(b0, b)) Some((a, b)) else Some((a0, b0))
      },
      _.map(_._1)
    )

  def head[A]: Fold[A, Option[A]] = of(Option.empty[A])((s, a) => s.orElse(Some(a)), identity)

  def last[A]: Fold[A, Option[A]] = of(Option.empty[A])((_, a) => Some(a), identity)

  def get[A](idx: Int): Fold[A, Option[A]] =
    of(Right(0): Either[A, Int])(
      { case (Right(k), a) => if (k == idx) Left(a) else Right(k + 1); case (Left(a), _) => Left(a) },
      { case Right(_) => None; case Left(a) => Some(a) }
    )

  def find[A](p: A => Boolean): Fold[A, Option[A]] =
    of(Option.empty[A])((s, a) => s.orElse(if (p(a)) Some(a) else None), identity)

  def findLast[A](p: A => Boolean): Fold[A, Option[A]] =
    of(Option.empty[A])((s, a) => (if (p(a)) Some(a) else None).orElse(s), identity)

  def length[A]: Fold[A, Int] = of(0)((s, _) => s + 1, identity)

  def isEmpty[A]: Fold[A, Boolean] = of(true)((_, _) => false, identity)

  def nonEmpty[A]: Fold[A, Boolean] = isEmpty.rmap(!_)

  def toList[A]: Fold[A, List[A]] = of(List.empty[A])(_ :+ _, identity)

  def toVector[A]: Fold[A, Vector[A]] = of(Vector.empty[A])(_ :+ _, identity)

  def toMap[A, B]: Fold[(A, B), Map[A, B]] = of(Map.empty[A, B])(_ + _, identity)
}

private[tatami] trait FoldInstances {

  /**
    * Instances of `cats` types for [[Fold]] type.
    */
  implicit def catsInstancesForFold[A]: Comonad[Fold[A, *]] with Applicative[Fold[A, *]] =
    new Comonad[Fold[A, *]] with Applicative[Fold[A, *]] {
      override def map[B, C](fb: Fold[A, B])(f: B => C): Fold[A, C] =
        Fold.of(fb.start)(fb.step(_, _), s => f(fb.end(s)))

      def pure[B](b: B): Fold[A, B] = Fold.of(())((_, _) => (), _ => b)
      def ap[B, C](ff: Fold[A, B => C])(fb: Fold[A, B]): Fold[A, C] =
        Fold.of((ff.start, fb.start))(
          { case ((sf, sb), a) => (ff.step(sf, a), fb.step(sb, a)) },
          { case (sf, sb) => ff.end(sf)(fb.end(sb)) }
        )

      def extract[B](fb: Fold[A, B]): B = fb.end(fb.start)
      def coflatMap[B, C](fb: Fold[A, B])(f: Fold[A, B] => C): Fold[A, C] =
        Fold.of(fb.start)(fb.step(_, _), s => f(Fold.of(s)(fb.step(_, _), fb.end(_))))
    }

  /**
    * Instances of `cats.arrow` types for [[Fold]].
    *
    * For now there is no `Choice` instance, because cats' `Choice` inherits `Category`,
    * but [[Fold]] has no `identity` implementation.
    */
  implicit val catsArrowInstancesForFold: Compose[Fold] with Profunctor[Fold] =
    new Compose[Fold] with Profunctor[Fold] {
      def compose[A, B, C](f: Fold[B, C], g: Fold[A, B]): Fold[A, C] =
        g.andThen(f)

      def dimap[A, B, C, D](fab: Fold[A, B])(f: C => A)(g: B => D): Fold[C, D] =
        fab.dimap(f)(g)
    }
}
