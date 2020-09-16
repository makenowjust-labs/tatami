package codes.quine.labo
package tatami

import cats.Monad
import cats.syntax.eq._
import minitest.SimpleTestSuite

import hariko.Property
import hariko.minitest.HarikoChecker
import TestImplicits._
import Request._

object RequestSuite extends SimpleTestSuite with HarikoChecker {
  test("Request#lmap") {
    val done = Done[Option, Int, Int](Some(100))
    val pull = Pull[Option, Int, Int](z => Some(z))
    assert(done.lmap((_: Int) + 1) === done)
    assert(pull.lmap((_: Int) + 1) === Pull(z => Some(z + 1)))
  }

  test("Request#lmap: Contravariant") {
    // identity:
    check(Property.forAll { fa: Request[Option, Int, Int] =>
      fa.lmap(identity[Int]) === fa
    })
    // composition:
    check(Property.forAll[(Request[Option, Int, Int], Int => Int, Int => Int)] {
      case (fa, f, g) =>
        fa.lmap(f.andThen(g)) === fa.lmap(g).lmap(f)
    })
  }

  test("Request#lmapM") {
    val done = Done[Option, Int, Int](Some(100))
    val pull = Pull[Option, Int, Int](z => Some(z))
    assert(done.lmapM((x: Int) => Option(x + 1)) === done)
    assert(pull.lmapM((x: Int) => Option(x + 1)) === Pull(z => Some(z + 1)))
  }

  test("Request#lmapM: Contravariant") {
    // identity:
    check(Property.forAll { fa: Request[Option, Int, Int] =>
      fa.lmapM(Monad[Option].pure(_: Int)) === fa
    })
    // composition:
    check(Property.forAll[(Request[Option, Int, Int], Int => Option[Int], Int => Option[Int])] {
      case (fa, f, g) =>
        fa.lmapM((x: Int) => f(x).flatMap(g)) === fa.lmapM(g).lmapM(f)
    })
  }

  test("Request#rmap") {
    val done = Done[Option, Int, Int](Some(100))
    val pull = Pull[Option, Int, Int](z => Some(z))
    assert(done.rmap(_ + 1) === Done(Some(101)))
    assert(pull.rmap(_ + 1) === Pull(z => Some(z + 1)))
  }

  test("Request#rmap: Functor") {
    // identity:
    check(Property.forAll { fa: Request[Option, Int, Int] =>
      fa.rmap(identity) === fa
    })
    // composition:
    check(Property.forAll[(Request[Option, Int, Int], Int => Int, Int => Int)] {
      case (fa, f, g) =>
        fa.rmap(f.andThen(g)) === fa.rmap(f).rmap(g)
    })
  }
}
