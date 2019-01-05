package analytics.classes

import analytics.Fn

trait MonoidFn[A] {
  def combine: (A, A) Fn A
  def empty: A
}

object MonoidFn {
  def apply[A: MonoidFn]: MonoidFn[A] = implicitly

  def create[A](c: (A, A) Fn A, e: A): MonoidFn[A] = new MonoidFn[A] {
    def combine: Fn[(A, A), A] = c
    def empty: A = e
  }
}

trait CommutativeMonoidFn[A] extends MonoidFn[A]

object CommutativeMonoidFn {
  implicit def commutativeMonoidInt: CommutativeMonoidFn[Int] = new CommutativeMonoidFn[Int] {
    def combine: Fn[(Int, Int), Int] = Fn.Plus

    def empty: Int = 0
  }

  def apply[A: CommutativeMonoidFn]: CommutativeMonoidFn[A] = implicitly

  def create[A](c: (A, A) Fn A, e: A): CommutativeMonoidFn[A] = new CommutativeMonoidFn[A] {
    def combine: Fn[(A, A), A] = c
    def empty: A = e
  }
}
