package codes.quine.labo.tatami

import cats.Applicative
import cats.Bimonad
import cats.CoflatMap
import cats.Comonad
import cats.FlatMap
import cats.Foldable
import cats.Functor
import cats.arrow.Compose
import cats.arrow.Profunctor

trait Fold[M[_], A, B] extends Serializable { fab =>
  type S

  def start: M[S]
  def step(s: S, a: A): M[S]
  def end(s: S): M[B]

  def apply[F[_]](fa: F[A])(implicit F: Foldable[F], M: FlatMap[M]): M[B] =
    M.flatMap(F.foldLeft(fa, start)((s, a) => M.flatMap(s)(step(_, a))))(end(_))

  def map[C](f: B => C)(implicit M: Functor[M]): Fold[M, A, C] =
    Fold.of(fab.start)(fab.step(_, _), s => M.map(fab.end(s))(f))

  def lmap[Z](f: Z => A): Fold[M, Z, B] =
    Fold.of(fab.start)((s, z) => fab.step(s, f(z)), fab.end(_))

  def rmap[C](f: B => C)(implicit M: Functor[M]): Fold[M, A, C] =
    fab.map(f)

  def dimap[Z, C](f: Z => A)(g: B => C)(implicit M: Functor[M]): Fold[M, Z, C] =
    Fold.of(fab.start)((s, z) => fab.step(s, f(z)), s => M.map(fab.end(s))(g))

  def andThen[C](fbc: Fold[M, B, C])(implicit M: FlatMap[M]): Fold[M, A, C] =
    Fold.of(M.tuple2(fab.start, fbc.start))(
      {
        case ((s0, t0), a) =>
          M.flatMap(fab.step(s0, a))(s1 => M.flatMap(fab.end(s1))(b => M.map(fbc.step(t0, b))((s1, _))))
      },
      { case (_, t) => fbc.end(t) }
    )

  def compose[Z](fza: Fold[M, Z, A])(implicit M: FlatMap[M]): Fold[M, Z, B] =
    fza.andThen(fab)

  def split[C, D](fcd: Fold[M, C, D])(implicit M: Applicative[M]): Fold[M, (A, C), (B, D)] =
    Fold.of(M.tuple2(fab.start, fcd.start))(
      { case ((s, t), (a, c)) => M.tuple2(fab.step(s, a), fcd.step(t, c)) },
      { case (s, t) => M.tuple2(fab.end(s), fcd.end(t)) }
    )

  def ***[C, D](fcd: Fold[M, C, D])(implicit M: Applicative[M]): Fold[M, (A, C), (B, D)] =
    fab.split(fcd)

  def merge[C](fac: Fold[M, A, C])(implicit M: Applicative[M]): Fold[M, A, (B, C)] =
    Fold.of(M.tuple2(fab.start, fac.start))(
      { case ((sab, sac), a) => M.tuple2(fab.step(sab, a), fac.step(sac, a)) },
      { case (sab, sac) => M.tuple2(fab.end(sab), fac.end(sac)) }
    )

  def &&&[C](fac: Fold[M, A, C])(implicit M: Applicative[M]): Fold[M, A, (B, C)] =
    fab.merge(fac)
}

object Fold extends FoldInstances0 {
  type Aux[M[_], A, B, S0] = Fold[M, A, B] { type S = S0 }

  def of[M[_], A, B, S0](i: => M[S0])(f: (S0, A) => M[S0], g: S0 => M[B]): Aux[M, A, B, S0] =
    new Fold[M, A, B] {
      type S = S0
      def start: M[S0] = i
      def step(s: S0, a: A): M[S0] = f(s, a)
      def end(s: S0): M[B] = g(s)
    }

  def forallM[M[_], A](f: A => M[Boolean])(implicit M: Applicative[M]): Fold[M, A, Boolean] =
    of(M.pure(true))((s, a) => if (s) f(a) else M.pure(false), M.pure(_))

  def existsM[M[_], A](f: A => M[Boolean])(implicit M: Applicative[M]): Fold[M, A, Boolean] =
    of(M.pure(false))((s, a) => if (s) M.pure(true) else f(a), M.pure(_))

  def forall[M[_], A](f: A => Boolean)(implicit M: Applicative[M]): Fold[M, A, Boolean] =
    forallM(a => M.pure(f(a)))

  def exists[M[_], A](f: A => Boolean)(implicit M: Applicative[M]): Fold[M, A, Boolean] =
    existsM(a => M.pure(f(a)))

  def head[M[_], A](implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    of(M.pure(Option.empty[A]))((s, a) => M.pure(s.orElse(Some(a))), M.pure(_))

  def last[M[_], A](implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    of(M.pure(Option.empty[A]))((_, a) => M.pure(Some(a)), M.pure(_))

  def findM[M[_], A](f: A => M[Boolean])(implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    of(M.pure(Option.empty[A]))(
      {
        case (None, a)    => M.map(f(a))(if (_) Some(a) else None)
        case (Some(a), _) => M.pure(Some(a))
      },
      M.pure(_)
    )

  def findLastM[M[_], A](f: A => M[Boolean])(implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    of(M.pure(Option.empty[A]))(
      (s, a) => M.map(f(a))(x => (if (x) Some(a) else None).orElse(s)),
      M.pure(_)
    )

  def find[M[_], A](f: A => Boolean)(implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    findM(a => M.pure(f(a)))

  def findLast[M[_], A](f: A => Boolean)(implicit M: Applicative[M]): Fold[M, A, Option[A]] =
    findLastM(a => M.pure(f(a)))

  def toList[M[_], A](implicit M: Applicative[M]): Fold[M, A, List[A]] =
    of(M.pure(List.empty[A]))((s, a) => M.pure(s :+ a), M.pure(_))
}

private[tatami] trait FoldInstances0 extends FoldInstances1 {
  implicit def catsApplicativeInstanceForFold[M[_], A](implicit M: Applicative[M]): Applicative[Fold[M, A, *]] =
    new FoldApplicative[M, A] { val applicative = M }

  implicit def catsComonadInstanceForFold[M[_], A](implicit M: Bimonad[M]): Comonad[Fold[M, A, *]] =
    new FoldComonad[M, A] { val bimonad = M }

  implicit def catsComposeInstanceForFold[M[_]](implicit M: FlatMap[M]): Compose[Fold[M, *, *]] =
    new FoldCompose[M] { val flatMap = M }

  implicit def catsProfunctorInstanceForFold[M[_]](implicit M: Functor[M]): Profunctor[Fold[M, *, *]] =
    new FoldProfunctor[M] { val functor = M }
}

private[tatami] trait FoldInstances1 {
  implicit def catsFunctorInstanceForFold[M[_], A](implicit M: Functor[M]): Functor[Fold[M, A, *]] =
    new FoldFunctor[M, A] { val functor = M }

  implicit def catsCoflatMapInstanceForFold[M[_], A](implicit M: Applicative[M]): CoflatMap[Fold[M, A, *]] =
    new FoldCoflatMap[M, A] { val applicative = M }
}

private[tatami] trait FoldFunctor[M[_], A] extends Functor[Fold[M, A, *]] {
  implicit val functor: Functor[M]
  override def map[B, C](fb: Fold[M, A, B])(f: B => C): Fold[M, A, C] = fb.map(f)
}

private[tatami] trait FoldApplicative[M[_], A] extends Applicative[Fold[M, A, *]] with FoldFunctor[M, A] {
  implicit val applicative: Applicative[M]
  lazy val functor: Functor[M] = applicative
  def pure[B](b: B): Fold[M, A, B] =
    Fold.of(applicative.pure(()))((_, _) => applicative.pure(()), _ => applicative.pure(b))
  def ap[B, C](ff: Fold[M, A, B => C])(fb: Fold[M, A, B]): Fold[M, A, C] =
    (ff &&& fb).map { case (f, b) => f(b) }
}

private[tatami] trait FoldCoflatMap[M[_], A] extends CoflatMap[Fold[M, A, *]] {
  implicit val applicative: Applicative[M]
  def map[B, C](fab: Fold[M, A, B])(f: B => C): Fold[M, A, C] = fab.map(f)
  def coflatMap[B, C](fab: Fold[M, A, B])(f: Fold[M, A, B] => C): Fold[M, A, C] =
    Fold.of(fab.start)(
      fab.step(_, _),
      s => applicative.pure(f(Fold.of(applicative.pure(s))(fab.step(_, _), fab.end(_))))
    )
}

private[tatami] trait FoldComonad[M[_], A] extends Comonad[Fold[M, A, *]] with FoldCoflatMap[M, A] {
  implicit val bimonad: Bimonad[M]
  lazy val applicative = bimonad
  def extract[B](fb: Fold[M, A, B]): B = bimonad.extract(bimonad.flatMap(fb.start)(fb.end(_)))
}

private[tatami] trait FoldCompose[M[_]] extends Compose[Fold[M, *, *]] {
  implicit val flatMap: FlatMap[M]
  def compose[A, B, C](fbc: Fold[M, B, C], fab: Fold[M, A, B]): Fold[M, A, C] = fbc.compose(fab)
}

private[tatami] trait FoldProfunctor[M[_]] extends Profunctor[Fold[M, *, *]] {
  implicit val functor: Functor[M]
  def dimap[A, B, C, D](fab: Fold[M, A, B])(f: C => A)(g: B => D): Fold[M, C, D] = fab.dimap(f)(g)
}
