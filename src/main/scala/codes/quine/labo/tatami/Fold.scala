package codes.quine.labo.tatami

import cats.Applicative
import cats.Bimonad
import cats.CoflatMap
import cats.Comonad
import cats.FlatMap
import cats.Foldable
import cats.Functor
import cats.Monad
import cats.arrow.Compose
import cats.arrow.Profunctor
import cats.syntax.flatMap._
import cats.syntax.functor._

import Request._

trait Fold[M[_], A, B] extends Serializable { fab =>
  type S

  def start: M[S]
  def step(s: S): M[Request[M, A, S]]
  def end(s: S): M[B]

  def lmap[Z](f: Z => A)(implicit M: Functor[M]): Fold[M, Z, B] =
    new Fold[M, Z, B] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, Z, S]] = fab.step(s).map(_.lmap(f))
      def end(s: S): M[B] = fab.end(s)
    }

  def lfilter(f: A => Boolean)(implicit M: Applicative[M]): Fold[M, A, B] =
    new Fold[M, A, B] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, A, S]] =
        fab.step(s).map {
          case Done(ms) => Done(ms)
          case Pull(k)  => Pull(a => if (f(a)) k(a) else M.pure(s))
        }
      def end(s: S): M[B] = fab.end(s)
    }

  def lmapFilter[Z](f: Z => Option[A])(implicit M: Applicative[M]): Fold[M, Z, B] =
    new Fold[M, Z, B] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, Z, S]] =
        fab.step(s).map {
          case Done(ms) => Done(ms)
          case Pull(k) =>
            Pull(f(_) match {
              case None    => M.pure(s)
              case Some(a) => k(a)
            })
        }
      def end(s: S): M[B] = fab.end(s)
    }

  def lcollect[Z](f: PartialFunction[Z, A])(implicit M: Applicative[M]): Fold[M, Z, B] =
    fab.lmapFilter(f.lift)

  def lmapM[Z](f: Z => M[A])(implicit M: FlatMap[M]): Fold[M, Z, B] =
    new Fold[M, Z, B] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, Z, S]] = fab.step(s).map(_.lmapM(f))
      def end(s: S): M[B] = fab.end(s)
    }

  def lfilterM(f: A => M[Boolean])(implicit M: Monad[M]): Fold[M, A, B] =
    new Fold[M, A, B] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, A, S]] =
        fab.step(s).map {
          case Done(ms) => Done(ms)
          case Pull(k)  => Pull(a => M.ifM(f(a))(k(a), M.pure(s)))
        }
      def end(s: S): M[B] = fab.end(s)
    }

  def lmapFilterM[Z](f: Z => M[Option[A]])(implicit M: Monad[M]): Fold[M, Z, B] =
    new Fold[M, Z, B] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, Z, S]] =
        fab.step(s).map {
          case Done(ms) => Done(ms)
          case Pull(k) =>
            Pull(f(_).flatMap {
              case None    => M.pure(s)
              case Some(a) => k(a)
            })
        }
      def end(s: S): M[B] = fab.end(s)
    }

  def rmap[C](f: B => C)(implicit M: Functor[M]): Fold[M, A, C] =
    new Fold[M, A, C] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, A, S]] = fab.step(s)
      def end(s: S): M[C] = fab.end(s).map(f)
    }

  def rmapM[C](f: B => M[C])(implicit M: FlatMap[M]): Fold[M, A, C] =
    new Fold[M, A, C] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, A, S]] = fab.step(s)
      def end(s: S): M[C] = fab.end(s).flatMap(f)
    }

  def dimap[Z, C](f: Z => A)(g: B => C)(implicit M: Functor[M]): Fold[M, Z, C] =
    new Fold[M, Z, C] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, Z, S]] = fab.step(s).map(_.lmap(f))
      def end(s: S): M[C] = fab.end(s).map(g)
    }

  def andThen[C](fbc: Fold[M, B, C])(implicit M: Monad[M]): Fold[M, A, C] =
    new Fold[M, A, C] {
      type S = (fab.S, fbc.S)
      def start: M[S] = M.tuple2(fab.start, fbc.start)
      def step(s: S): M[Request[M, A, S]] =
        fab.step(s._1).flatMap {
          case Done(ms1) => M.pure(Done(ms1.map((_, s._2))))
          case Pull(k1) =>
            fbc.step(s._2).flatMap {
              case Done(ms2) => M.pure(Done(ms2.map((s._1, _))))
              case Pull(k2) =>
                M.pure(Pull { a =>
                  for {
                    s1 <- k1(a)
                    b <- fab.end(s1)
                    s2 <- k2(b)
                  } yield (s1, s2)
                })
            }
        }
      def end(s: S): M[C] = fbc.end(s._2)
    }

  def compose[Z](fza: Fold[M, Z, A])(implicit M: Monad[M]): Fold[M, Z, B] =
    fza.andThen(fab)

  def split[C, D](fcd: Fold[M, C, D])(implicit M: Applicative[M]): Fold[M, (A, C), (B, D)] =
    new Fold[M, (A, C), (B, D)] {
      type S = (fab.S, fcd.S)
      def start: M[S] = M.tuple2(fab.start, fcd.start)
      def step(s: S): M[Request[M, (A, C), S]] =
        M.map2(fab.step(s._1), fcd.step(s._2)) {
          case (Done(ms1), Done(ms2)) => Done(M.tuple2(ms1, ms2))
          case (Done(ms1), Pull(k2))  => Pull { case (_, c) => M.tuple2(ms1, k2(c)) }
          case (Pull(k1), Done(ms2))  => Pull { case (a, _) => M.tuple2(k1(a), ms2) }
          case (Pull(k1), Pull(k2))   => Pull { case (a, c) => M.tuple2(k1(a), k2(c)) }
        }
      def end(s: S): M[(B, D)] = M.tuple2(fab.end(s._1), fcd.end(s._2))
    }

  def merge[C](fac: Fold[M, A, C])(implicit M: Applicative[M]): Fold[M, A, (B, C)] =
    new Fold[M, A, (B, C)] {
      type S = (fab.S, fac.S)
      def start: M[S] = M.tuple2(fab.start, fac.start)
      def step(s: S): M[Request[M, A, S]] =
        M.map2(fab.step(s._1), fac.step(s._2)) {
          case (Done(ms1), Done(ms2)) => Done(M.tuple2(ms1, ms2))
          case (Done(ms1), Pull(k2))  => Pull { a => M.tuple2(ms1, k2(a)) }
          case (Pull(k1), Done(ms2))  => Pull { a => M.tuple2(k1(a), ms2) }
          case (Pull(k1), Pull(k2))   => Pull { a => M.tuple2(k1(a), k2(a)) }
        }
      def end(s: S): M[(B, C)] = M.tuple2(fab.end(s._1), fac.end(s._2))
    }

  def combine(fab2: Fold[M, A, B])(implicit M: Monad[M]): Fold[M, A, B] =
    new Fold[M, A, B] {
      type S = Either[fab.S, fab2.S]
      def start: M[S] = fab.start.map(Left(_))
      def step(s: S): M[Request[M, A, S]] =
        s match {
          case Left(s1) =>
            fab.step(s1).flatMap {
              case Done(_) => fab2.start.map(Right(_)).flatMap(step(_))
              case Pull(k) => M.pure(Pull(a => k(a).map(Left(_))))
            }
          case Right(s2) => fab2.step(s2).map(_.rmap(Right(_)))
        }
      def end(s: S): M[B] = s.fold(fab.end(_), fab2.end(_))
    }
}

object Fold extends FoldInstances0 {
  def apply[F[_], M[_], A, B](fa: F[A], fab: Fold[M, A, B])(implicit F: Foldable[F], M: Monad[M]): M[(B, Source[A])] = {
    type S = fab.S
    def foldUntil(fa: F[A], s0: S)(f: S => M[Request[M, A, S]]): M[(S, Source[A])] =
      M.tailRecM[(S, Source[A]), (S, Source[A])]((s0, Source.from(fa))) {
        case (s1, src1) =>
          f(s1).flatMap {
            case Done(ms2) => ms2.map(s2 => Right(s2, src1))
            case Pull(k) =>
              src1.uncons match {
                case None            => M.pure(Right((s1, src1)))
                case Some((a, src2)) => k(a).map(s2 => Left((s2, src2)))
              }
          }
      }

    for {
      s0 <- fab.start
      (s1, src) <- foldUntil(fa, s0)(fab.step(_))
      b <- fab.end(s1)
    } yield (b, src)
  }

  def pure[M[_], A, B](b: B)(implicit M: Applicative[M]): Fold[M, A, B] =
    new Fold[M, A, B] {
      type S = Unit
      def start: M[S] = M.pure(())
      def step(s: S): M[Request[M, A, S]] = M.pure(Done(M.pure(())))
      def end(s: S): M[B] = M.pure(b)
    }

  def head[M[_], A](implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    new Fold[M, A, Option[A]] {
      type S = Option[A]
      def start: M[S] = M.pure(Option.empty)
      def step(s: S): M[Request[M, A, S]] =
        if (s.isEmpty) M.pure(Pull(a => M.pure(Some(a))))
        else M.pure(Done(M.pure(s)))
      def end(s: S): M[Option[A]] = M.pure(s)
    }

  def last[M[_], A](implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    new Fold[M, A, Option[A]] {
      type S = Option[A]
      def start: M[S] = M.pure(Option.empty)
      def step(s: S): M[Request[M, A, S]] =
        M.pure(Pull(a => M.pure(Some(a))))
      def end(s: S): M[Option[A]] = M.pure(s)
    }

  def toList[M[_], A](implicit M: Applicative[M]): Fold[M, A, List[A]] =
    new Fold[M, A, List[A]] {
      type S = List[A]
      def start: M[S] = M.pure(List.empty)
      def step(s: S): M[Request[M, A, S]] =
        M.pure(Pull(a => M.pure(s :+ a)))
      def end(s: S): M[List[A]] = M.pure(s)
    }
}

private[tatami] trait FoldInstances0 extends FoldInstances1 {
  implicit def catsComonadInstanceForFold[M[_], A](implicit M: Bimonad[M]): Comonad[Fold[M, A, *]] =
    new FoldComonad[M, A] { val bimonad = M }

  implicit def catsComposeInstanceForFold[M[_]](implicit M: Monad[M]): Compose[Fold[M, *, *]] =
    new FoldCompose[M] { val monad = M }

  implicit def catsProfunctorInstanceForFold[M[_]](implicit M: Functor[M]): Profunctor[Fold[M, *, *]] =
    new FoldProfunctor[M] { val functor = M }
}

private[tatami] trait FoldInstances1 extends FoldInstances2 {
  implicit def catsCoflatMapInstanceForFold[M[_], A](implicit M: Applicative[M]): CoflatMap[Fold[M, A, *]] =
    new FoldCoflatMap[M, A] { val applicative = M }
}

private[tatami] trait FoldInstances2 extends FoldInstances3 {
  implicit def catsApplicativeInstanceForFold[M[_], A](implicit M: Applicative[M]): Applicative[Fold[M, A, *]] =
    new FoldApplicative[M, A] { val applicative = M }
}

private[tatami] trait FoldInstances3 {
  implicit def catsFunctorInstanceForFold[M[_], A](implicit M: Functor[M]): Functor[Fold[M, A, *]] =
    new FoldFunctor[M, A] { val functor = M }
}

private[tatami] trait FoldFunctor[M[_], A] extends Functor[Fold[M, A, *]] {
  implicit val functor: Functor[M]
  override def map[B, C](fb: Fold[M, A, B])(f: B => C): Fold[M, A, C] = fb.rmap(f)
}

private[tatami] trait FoldApplicative[M[_], A] extends Applicative[Fold[M, A, *]] with FoldFunctor[M, A] {
  implicit val applicative: Applicative[M]
  lazy val functor: Functor[M] = applicative
  def pure[B](b: B): Fold[M, A, B] = Fold.pure(b)
  def ap[B, C](ff: Fold[M, A, B => C])(fb: Fold[M, A, B]): Fold[M, A, C] =
    ff.merge(fb).rmap { case (f, b) => f(b) }
}

private[tatami] trait FoldCoflatMap[M[_], A] extends CoflatMap[Fold[M, A, *]] with FoldFunctor[M, A] {
  implicit val applicative: Applicative[M]
  lazy val functor: Functor[M] = applicative
  def coflatMap[B, C](fab: Fold[M, A, B])(f: Fold[M, A, B] => C): Fold[M, A, C] =
    new Fold[M, A, C] {
      type S = fab.S
      def start: M[S] = fab.start
      def step(s: S): M[Request[M, A, S]] = fab.step(s)
      def end(s: S): M[C] =
        applicative.pure(f(new Fold[M, A, B] {
          type S = fab.S
          def start: M[S] = applicative.pure(s)
          def step(s: S): M[Request[M, A, S]] = fab.step(s)
          def end(s: S): M[B] = fab.end(s)
        }))
    }
}

private[tatami] trait FoldComonad[M[_], A] extends Comonad[Fold[M, A, *]] with FoldCoflatMap[M, A] {
  implicit val bimonad: Bimonad[M]
  lazy val applicative: Applicative[M] = bimonad
  def extract[B](fb: Fold[M, A, B]): B = bimonad.extract(bimonad.flatMap(fb.start)(fb.end(_)))
}

private[tatami] trait FoldCompose[M[_]] extends Compose[Fold[M, *, *]] {
  implicit val monad: Monad[M]
  def compose[A, B, C](fbc: Fold[M, B, C], fab: Fold[M, A, B]): Fold[M, A, C] = fbc.compose(fab)
}

private[tatami] trait FoldProfunctor[M[_]] extends Profunctor[Fold[M, *, *]] {
  implicit val functor: Functor[M]
  override def lmap[A, B, C](fab: Fold[M, A, B])(f: C => A): Fold[M, C, B] = fab.lmap(f)
  override def rmap[A, B, C](fab: Fold[M, A, B])(f: B => C): Fold[M, A, C] = fab.rmap(f)
  def dimap[A, B, C, D](fab: Fold[M, A, B])(f: C => A)(g: B => D): Fold[M, C, D] = fab.dimap(f)(g)
}
