package codes.quine.labo
package tatami

import cats.arrow.Category
import cats.arrow.Profunctor
import cats.syntax.eq._
import cats.syntax.functor._
import minitest.SimpleTestSuite

import hariko.minitest.HarikoChecker
import hariko.Property
import TestImplicits._

object TransSuite extends SimpleTestSuite with HarikoChecker {
  test("Trans: Functor") {
    type M[A] = Trans[Option, Boolean, A]
    // identity:
    check(Property.forAll { fa: M[Boolean] =>
      fa.map(identity) === fa
    })
    // composition:
    check(Property.forAll[(M[Boolean], Boolean => Boolean, Boolean => Boolean)] {
      case (fa, f, g) =>
        fa.map(f).map(g) === fa.map(f.andThen(g))
    })
  }

  test("Trans: Profunctor") {
    type =>:[A, B] = Trans[Option, A, B]
    val M = Profunctor[=>:]
    // identity:
    check(Property.forAll { (fab: Boolean =>: Boolean) =>
      M.dimap(fab)(identity[Boolean])(identity[Boolean]) === fab
    })
    // composition:
    check(
      Property.forAll[
        (Boolean =>: Boolean, (Boolean => Boolean, Boolean => Boolean, Boolean => Boolean, Boolean => Boolean))
      ] {
        case (fab, (f1, f2, g1, g2)) =>
          M.dimap(M.dimap(fab)(f1)(g1))(f2)(g2) === M.dimap(fab)(f2.andThen(f1))(g1.andThen(g2))
      }
    )
  }

  test("Trans: Category") {
    type =>:[A, B] = Trans[Option, A, B]
    val M = Category[=>:]
    // identity:
    check(Property.forAll { f: Boolean =>: Boolean =>
      M.andThen(f, M.id[Boolean]) === f && M.andThen(M.id[Boolean], f) === f
    })
    // associativity:
    check(Property.forAll[(Boolean =>: Boolean, Boolean =>: Boolean, Boolean =>: Boolean)] {
      case (f, g, h) =>
        M.andThen(f, M.andThen(g, h)) === M.andThen(M.andThen(f, g), h)
    })
  }
}
