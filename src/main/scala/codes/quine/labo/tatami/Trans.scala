package codes.quine.labo.tatami

import cats.Applicative
import cats.FlatMap
import cats.Foldable
import cats.Functor
import cats.Monad
import cats.arrow.Category
import cats.arrow.Profunctor
import cats.syntax.functor._

import Request._

trait Trans[M[_], A, B] { tab =>
  def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C]

  def lmap[Z](f: Z => A)(implicit M: Functor[M]): Trans[M, Z, B] =
    new Trans[M, Z, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, Z, C] = tab.trans(fbc).lmap(f)
    }

  def rmap[C](f: B => C)(implicit M: Functor[M]): Trans[M, A, C] =
    new Trans[M, A, C] {
      def trans[D](fcd: Fold[M, C, D]): Fold[M, A, D] = tab.trans(fcd.lmap(f))
    }

  def dimap[Z, C](f: Z => A)(g: B => C)(implicit M: Functor[M]): Trans[M, Z, C] =
    new Trans[M, Z, C] {
      def trans[D](fcd: Fold[M, C, D]): Fold[M, Z, D] = tab.trans(fcd.lmap(g)).lmap(f)
    }

  def andThen[C](tbc: Trans[M, B, C]): Trans[M, A, C] =
    new Trans[M, A, C] {
      def trans[D](fcd: Fold[M, C, D]): Fold[M, A, D] = tab.trans(tbc.trans(fcd))
    }

  def compose[Z](tza: Trans[M, Z, A]): Trans[M, Z, B] =
    tza.andThen(tab)

  def split[C, D](tcd: Trans[M, C, D])(implicit M: Monad[M]): Trans[M, (A, C), (B, D)] =
    new Trans[M, (A, C), (B, D)] {
      def trans[E](fbde: Fold[M, (B, D), E]): Fold[M, (A, C), E] = {
        val fab = tab.trans(Fold.last[M, B])
        val fcd = tcd.trans(Fold.last[M, D])
        fab.split(fcd).andThen(fbde.lcollect { case (Some(b), Some(d)) => (b, d) })
      }
    }

  def merge[C](tac: Trans[M, A, C])(implicit M: Monad[M]): Trans[M, A, (B, C)] =
    new Trans[M, A, (B, C)] {
      def trans[D](fbcd: Fold[M, (B, C), D]): Fold[M, A, D] = {
        val fab = tab.trans(Fold.last[M, B])
        val fac = tac.trans(Fold.last[M, C])
        fab.merge(fac).andThen(fbcd.lcollect { case (Some(b), Some(c)) => (b, c) })
      }
    }

  def combine(tab2: Trans[M, A, B])(implicit M: Monad[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] = {
        val fab = tab.trans(Fold.last[M, B])
        val fab2 = tab2.trans(Fold.last[M, B])
        fab.combine(fab2).andThen(fbc.lcollect { case Some(a) => a })
      }
    }
}

object Trans extends TransInstances0 {
  def apply[F[_]: Foldable, M[_]: Monad, A, B, C](
      fa: F[A],
      fbc: Fold[M, B, C],
      tab: Trans[M, A, B]
  ): M[(C, Source[A])] =
    Fold(fa, tab.trans(fbc))

  def pure[M[_], A, B](b: B)(implicit M: Monad[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] =
        fbc.lmap(_ => b)
    }

  def empty[M[_], A, B](implicit M: Applicative[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] =
        new Fold[M, A, C] {
          type S = fbc.S
          def start: M[S] = fbc.start
          def step(s: S): M[Request[M, A, S]] = M.pure(Done(M.pure(s)))
          def end(s: S): M[C] = fbc.end(s)
        }
    }

  def id[M[_], A]: Trans[M, A, A] =
    new Trans[M, A, A] {
      def trans[C](fac: Fold[M, A, C]): Fold[M, A, C] = fac
    }

  def map[M[_], A, B](f: A => B)(implicit M: Functor[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] = fbc.lmap(f)
    }

  def filter[M[_], A](f: A => Boolean)(implicit M: Applicative[M]): Trans[M, A, A] =
    new Trans[M, A, A] {
      def trans[C](fac: Fold[M, A, C]): Fold[M, A, C] = fac.lfilter(f)
    }

  def mapFilter[M[_], A, B](f: A => Option[B])(implicit M: Applicative[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] = fbc.lmapFilter(f)
    }

  def collect[M[_], A, B](f: PartialFunction[A, B])(implicit M: Applicative[M]): Trans[M, A, B] =
    mapFilter(f.lift)

  def mapM[M[_], A, B](f: A => M[B])(implicit M: FlatMap[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] = fbc.lmapM(f)
    }

  def filterM[M[_], A](f: A => M[Boolean])(implicit M: Monad[M]): Trans[M, A, A] =
    new Trans[M, A, A] {
      def trans[C](fac: Fold[M, A, C]): Fold[M, A, C] = fac.lfilterM(f)
    }

  def mapFilterM[M[_], A, B](f: A => M[Option[B]])(implicit M: Monad[M]): Trans[M, A, B] =
    new Trans[M, A, B] {
      def trans[C](fbc: Fold[M, B, C]): Fold[M, A, C] = fbc.lmapFilterM(f)
    }

  def takeM[M[_], A](mn: M[Int])(implicit M: Applicative[M]): Trans[M, A, A] =
    new Trans[M, A, A] {
      def trans[C](fac: Fold[M, A, C]): Fold[M, A, C] =
        new Fold[M, A, C] {
          type S = (Int, fac.S)
          def start: M[S] = M.tuple2(mn, fac.start)
          def step(s: S): M[Request[M, A, S]] =
            if (s._1 > 0) fac.step(s._2).map(_.rmap((s._1 - 1, _)))
            else M.pure(Done(M.pure((0, s._2))))
          def end(s: S): M[C] = fac.end(s._2)
        }
    }

  def take[M[_], A](n: Int)(implicit M: Applicative[M]): Trans[M, A, A] = takeM(M.pure(n))

  def dropM[M[_], A](mn: M[Int])(implicit M: Applicative[M]): Trans[M, A, A] =
    new Trans[M, A, A] {
      def trans[C](fac: Fold[M, A, C]): Fold[M, A, C] =
        new Fold[M, A, C] {
          type S = (Int, fac.S)
          def start: M[S] = M.tuple2(mn, fac.start)
          def step(s: S): M[Request[M, A, S]] =
            if (s._1 > 0) M.pure(Pull(_ => M.pure((s._1 - 1, s._2))))
            else fac.step(s._2).map(_.rmap((0, _)))
          def end(s: S): M[C] = fac.end(s._2)
        }
    }

  def drop[M[_], A](n: Int)(implicit M: Applicative[M]): Trans[M, A, A] = dropM(M.pure(n))

  def zip[M[_], A, B](bs: Seq[B])(implicit M: Applicative[M]): Trans[M, A, (A, B)] =
    new Trans[M, A, (A, B)] {
      def trans[C](fabc: Fold[M, (A, B), C]): Fold[M, A, C] =
        new Fold[M, A, C] {
          type S = (Seq[B], fabc.S)
          def start: M[S] = fabc.start.map((bs, _))
          def step(s: S): M[Request[M, A, S]] =
            if (s._1.isEmpty) M.pure(Done(M.pure(s)))
            else fabc.step(s._2).map(_.lmap((_: A, s._1.head)).rmap((s._1.tail, _)))
          def end(s: S): M[C] = fabc.end(s._2)
        }
    }

  def zipWithIndex[M[_]: Applicative, A]: Trans[M, A, (A, Int)] =
    zip(LazyList.iterate(0)(_ + 1))
}

private[tatami] trait TransInstances0 {
  implicit def catsFunctorInstanceForTrans[M[_], A](implicit M: Functor[M]): Functor[Trans[M, A, *]] =
    new TransFunctor[M, A] { val functor = M }

  implicit def catsCategoryInstanceForTrans[M[_]](implicit M: Functor[M]): Category[Trans[M, *, *]] =
    new TransCategory[M] { val functor = M }

  implicit def catsProfunctorInstanceForTrans[M[_]](implicit M: Functor[M]): Profunctor[Trans[M, *, *]] =
    new TransProfunctor[M] { val functor = M }
}

private[tatami] trait TransFunctor[M[_], A] extends Functor[Trans[M, A, *]] {
  implicit val functor: Functor[M]
  override def map[B, C](tab: Trans[M, A, B])(f: B => C): Trans[M, A, C] = tab.rmap(f)
}

private[tatami] trait TransProfunctor[M[_]] extends Profunctor[Trans[M, *, *]] {
  implicit val functor: Functor[M]
  override def lmap[A, B, C](tab: Trans[M, A, B])(f: C => A): Trans[M, C, B] = tab.lmap(f)
  override def rmap[A, B, C](tab: Trans[M, A, B])(f: B => C): Trans[M, A, C] = tab.rmap(f)
  def dimap[A, B, C, D](tab: Trans[M, A, B])(f: C => A)(g: B => D): Trans[M, C, D] = tab.dimap(f)(g)
}

private[tatami] trait TransCategory[M[_]] extends Category[Trans[M, *, *]] {
  implicit val functor: Functor[M]
  def id[A]: Trans[M, A, A] = Trans.id[M, A]
  def compose[A, B, C](tbc: Trans[M, B, C], tab: Trans[M, A, B]): Trans[M, A, C] = tbc.compose(tab)
  override def andThen[A, B, C](tab: Trans[M, A, B], tbc: Trans[M, B, C]): Trans[M, A, C] = tab.andThen(tbc)
}
