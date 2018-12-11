package analytics

trait DatasetOpProgram[A, B] {
  def apply[F[_, _]](F: DatasetOp[F]): F[A, B]
}

trait DatasetOpProgramErr[A, B] {
  def apply[F[_, _]](F: DatasetOp[F]): Either[AnalyticsError, F[A, B]]
}

trait DatasetFoldProgram[A] {
  def apply[F[_]](F: DatasetFold[F]): F[A]
}

trait DatasetFoldProgramErr[A] {
  def apply[F[_]](F: DatasetFold[F]): Either[AnalyticsError, F[A]]
}
