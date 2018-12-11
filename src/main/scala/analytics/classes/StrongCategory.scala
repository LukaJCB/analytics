package analytics.classes

trait StrongCategory[F[_, _]] {

  def id[A]: F[A, A]

  def compose[A, B, C](f: F[A, B], g: F[B, C]): F[A, C]

  def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C]

  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]

  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]

  def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)]

  def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]]

  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    compose(first[A, B, C](f), second[C, D, B](g))

}

object StrongCategory {
  def apply[F[_, _]: StrongCategory]: StrongCategory[F] = implicitly
}
