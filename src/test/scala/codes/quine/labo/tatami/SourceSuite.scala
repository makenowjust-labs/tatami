package codes.quine.labo
package tatami

import cats.Alternative
import cats.Id
import cats.Monad
import cats.data.Nested
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import minitest.SimpleTestSuite

import hariko.Property
import hariko.minitest.HarikoChecker
import TestImplicits._

object SourceSuite extends SimpleTestSuite with HarikoChecker {
  test("Source: Functor") {
    // identity:
    check(Property.forAll { fa: Source[Int] =>
      fa.map(identity) === fa
    })
    // composition:
    check(Property.forAll[(Source[Int], Int => Int, Int => Int)] { case (fa, f, g) =>
      fa.map(f).map(g) === fa.map(f.andThen(g))
    })
  }

  test("Source: Monad") {
    val M = Monad[Source]
    // left identity
    check(Property.forAll[(Int, Int => Source[Int])] { case (a, f) =>
      M.pure(a).flatMap(f) === f(a)
    })
    // right identity:
    check(Property.forAll { fa: Source[Int] =>
      fa.flatMap(M.pure(_)) === fa
    })
    // associativity:
    check(
      Property
        .forAll[(Source[Int], Int => Source[Int], Int => Source[Int])] { case (fa, f, g) =>
          fa.flatMap(a => f(a).flatMap(g)) === fa.flatMap(f).flatMap(g)
        }
        .withParam(_.copy(maxScale = 50))
    )
    // tailRecM stack safety:
    val n = 50000
    val res = M.tailRecM(0)(i => M.pure(if (i < n) Left(i + 1) else Right(i)))
    assert(res === M.pure(n))
  }

  test("Source: Alternative") {
    val M = Alternative[Source]
    // zero:
    check(Property.forAll { fa: Source[Int] =>
      M.combineK(fa, M.empty) === fa && M.combineK(M.empty, fa) === fa
    })
    // associativityy:
    check(Property.forAll[(Source[Int], Source[Int], Source[Int])] { case (x, y, z) =>
      M.combineK(x, M.combineK(y, z)) === M.combineK(M.combineK(x, y), z)
    })
  }

  test("Source: Traverse") {
    // identity:
    check(Property.forAll { fa: Source[Int] =>
      fa.traverse[Id, Int](identity) === fa
    })
    // sequential composition:
    check(Property.forAll[(Source[Int], Int => Option[Int], Int => Option[Int])] { case (fa, f, g) =>
      val lhs = Nested(fa.traverse(f).map(_.traverse(g)))
      val rhs = fa.traverse[Nested[Option, Option, *], Int](a => Nested(f(a).map(g)))
      lhs === rhs
    })
    // TODO: write 'parallel composition' check.
  }
}
