import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import scala.collection.immutable.Queue

object InefficientProducerConsumer extends IOApp {

  def producer[F[_]: Sync: ContextShift](queueR: Ref[F, Queue[Int]], counter: Int): F[Unit] =
    (for {
      _ <- if (counter % 10000 == 0) Sync[F].delay(println(s"Produced $counter items")) else Sync[F].unit
      _ <- queueR.getAndUpdate(_.enqueue(counter + 1))
      _ <- ContextShift[F].shift
    } yield()) >> producer(queueR, counter + 1)

  def consumer[F[_]: Sync: ContextShift](queueR: Ref[F, Queue[Int]]): F[Unit] =
    (for {
      i0 <- queueR.modify { queue =>
        queue.dequeueOption.fold((queue, Option.empty[Int])) { case (i, queue) => (queue, Option(i)) }
      }
      _ <- if(i0.exists(_ % 10000 == 0)) Sync[F].delay(println(s"Consumed ${i0.get} items.")) else Sync[F].unit
      _ <- ContextShift[F].shift

    } yield i0) >> consumer(queueR)

  override def run(args: List[String]): IO[ExitCode] =
    for {
    queueR <- Ref.of[IO, Queue[Int]](Queue.empty[Int])
    res <- (consumer(queueR), producer(queueR, 0))
      .parMapN((_,_) => ExitCode.Success)
      .handleErrorWith {
        t =>
          IO(println(s"Error caught: ${t.getMessage}")).as(ExitCode.Error)
      }
    } yield res
}
