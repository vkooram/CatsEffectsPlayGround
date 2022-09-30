import cats.effect.{ContextShift, ExitCode, Fiber, IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object ThreadScheduling extends IOApp{
  val ecOne = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val csOne: ContextShift[IO] = IO.contextShift(ecOne)
  val ecTwo = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val csTwo: ContextShift[IO] = IO.contextShift(ecTwo)

  def infiniteIO(id: Int)(cs: ContextShift[IO]): IO[Fiber[IO, Unit]] = {
    def repeat: IO[Unit] = IO(println(id)).flatMap(_ => IO.shift *> repeat)
    repeat.start(cs)
  }
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- infiniteIO(1)(csOne)
      _ <- infiniteIO(11)(csOne)
      _ <- infiniteIO(2)(csTwo)
      _ <- infiniteIO(22)(csTwo)
    } yield (ExitCode.Success)
}
