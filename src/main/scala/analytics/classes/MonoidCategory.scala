package analytics.classes

trait MonoidCategory[A, F[_, _]] {
  def op: F[(A, A), A]
  def unit: A
}

object MonoidCategory {
  def apply[A, F[_, _]](implicit ev: MonoidCategory[A, F]): MonoidCategory[A, F] = ev
}


trait CommutativeMonoidCategory[A, F[_, _]] extends MonoidCategory[A, F]

object CommutativeMonoidCategory {
  def apply[A, F[_, _]](implicit ev: CommutativeMonoidCategory[A, F]): CommutativeMonoidCategory[A, F] = ev
}
