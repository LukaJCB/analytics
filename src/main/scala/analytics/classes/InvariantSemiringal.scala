package analytics.classes

trait InvariantSemiringal[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
  def zero: F[Nothing]
  def one: F[Unit]
}

object InvariantSemiringal {
  def apply[F[_]: InvariantSemiringal]: InvariantSemiringal[F] = implicitly
}