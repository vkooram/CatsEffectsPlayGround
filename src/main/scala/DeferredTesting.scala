import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Deferred
import cats.implicits.catsSyntaxParallelSequence1

object DeferredTesting extends IOApp {
  def start(d: Deferred[IO, Int]): IO[Unit] = {
    val attemptCompletion: Int => IO[Unit] = n => d.complete(n).attempt.void
    List(
      IO.race(
        attemptCompletion(2),
        attemptCompletion(1)
      ),
      d.get.flatMap(n=> IO(println(s"Result $n")))
    ).parSequence
      .void
  }
  override def run(args: List[String]): IO[ExitCode] =
    for {
      d <- Deferred[IO, Int]
      _ <- start(d)
    } yield (ExitCode.Success)
}
