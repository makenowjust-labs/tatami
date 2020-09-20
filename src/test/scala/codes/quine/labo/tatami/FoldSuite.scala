package codes.quine.labo
package tatami

import cats.Applicative
import cats.Comonad
import cats.Id
import cats.arrow.Compose
import cats.arrow.Profunctor
import cats.syntax.coflatMap._
import cats.syntax.eq._
import cats.syntax.functor._
import minitest.SimpleTestSuite

import hariko.Property
import hariko.minitest.HarikoChecker
import TestImplicits._

object FoldSuite extends SimpleTestSuite with HarikoChecker {
  test("Fold: Functor") {
    type M[A] = Fold[Id, Int, A]
    // identity:
    check(Property.forAll { fa: M[Boolean] =>
      fa.map(identity) === fa
    })
    // composition:
    check(Property.forAll[(M[Boolean], Boolean => Boolean, Boolean => Boolean)] { case (fa, f, g) =>
      fa.map(f).map(g) === fa.map(f.andThen(g))
    })
  }

  test("Fold: Applicative") {
    type M[A] = Fold[Option, Int, A]
    val M = Applicative[M]
    // left identity:
    check(Property.forAll { fa: M[Boolean] =>
      M.map2(M.unit, fa)((_, y) => y) === fa
    })
    // right identity:
    check(Property.forAll { fa: M[Boolean] =>
      M.map2(fa, M.unit)((x, _) => x) === fa
    })
    // associativity:
    check(Property.forAll[(M[Boolean], M[Boolean], M[Boolean])] { case (x, y, z) =>
      val lhs = M.map2(x, M.tuple2(y, z)) { case (x, (y, z)) => (x, y, z) }
      val rhs = M.map2(M.tuple2(x, y), z) { case ((x, y), z) => (x, y, z) }
      lhs === rhs
    })
  }

  test("Fold: CoflatMap") {
    type M[A] = Fold[Option, Boolean, A]
    // associativity:
    check(Property.forAll[(M[Boolean], M[Boolean] => Boolean, M[Boolean] => Boolean)] { case (fa, f, g) =>
      fa.coflatMap(f).coflatMap(g) === fa.coflatMap(x => g(x.coflatMap(f)))
    })
  }

  test("Fold: Comonad") {
    type M[A] = Fold[Id, Int, A]
    val M = Comonad[M]
    // left identity:
    check(Property.forAll { fa: M[Boolean] =>
      fa.coflatMap(M.extract(_)) === fa
    })
    // right identity:
    check(Property.forAll[(M[Boolean], M[Boolean] => Boolean)] { case (fa, f) =>
      M.extract(fa.coflatMap(f)) === f(fa)
    })
    // associativity:
    check(Property.forAll[(M[Boolean], M[Boolean] => Boolean, M[Boolean] => Boolean)] { case (fa, f, g) =>
      fa.coflatMap(f).coflatMap(g) === fa.coflatMap(x => g(x.coflatMap(f)))
    })
  }

  test("Fold: Compose") {
    type =>:[A, B] = Fold[Option, A, B]
    val M = Compose[=>:]
    // associativity:
    check(Property.forAll[(Boolean =>: Boolean, Boolean =>: Boolean, Boolean =>: Boolean)] { case (f, g, h) =>
      M.andThen(f, M.andThen(g, h)) === M.andThen(M.andThen(f, g), h)
    })
  }

  test("Fold: Profunctor") {
    type =>:[A, B] = Fold[Option, A, B]
    val M = Profunctor[=>:]
    // identity:
    check(Property.forAll { (fab: Boolean =>: Boolean) =>
      M.dimap(fab)(identity[Boolean])(identity[Boolean]) === fab
    })
    // composition:
    check(
      Property.forAll[
        (Boolean =>: Boolean, (Boolean => Boolean, Boolean => Boolean, Boolean => Boolean, Boolean => Boolean))
      ] { case (fab, (f1, f2, g1, g2)) =>
        M.dimap(M.dimap(fab)(f1)(g1))(f2)(g2) === M.dimap(fab)(f2.andThen(f1))(g1.andThen(g2))
      }
    )
  }
}
