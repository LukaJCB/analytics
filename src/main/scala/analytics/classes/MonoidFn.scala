package analytics.classes

import analytics.Fn

trait MonoidFn[A] {
  def combine: (A, A) Fn A
  def empty: A
}

object MonoidFn {
  def apply[A: MonoidFn]: MonoidFn[A] = implicitly
}

trait CommutativeMonoidFn[A] extends MonoidFn[A]

object CommutativeMonoidFn {
  implicit def commutativeMonoidInt: CommutativeMonoidFn[Int] = new CommutativeMonoidFn[Int] {
    def combine: Fn[(Int, Int), Int] = Fn.Plus

    def empty: Int = 0
  }

  def apply[A: CommutativeMonoidFn]: CommutativeMonoidFn[A] = implicitly
}
