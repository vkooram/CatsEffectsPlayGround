import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect._

import java.io.{File, FileInputStream, FileOutputStream}
import scala.concurrent.ExecutionContext

case class Result(count: Long, iter: Long)

object Main extends IOApp {

  def inputStream(file: File, guard: Semaphore[IO]): Resource[IO, FileInputStream] =
    Resource.make(
      IO(new FileInputStream(file))
    )(
      inputStream =>
        guard.withPermit{
          IO(inputStream.close()).handleErrorWith(_ => IO.unit)
        }
    )

  def outputStream(file: File, guard: Semaphore[IO]): Resource[IO, FileOutputStream] =
    Resource.make(
      IO(new FileOutputStream(file))
    )(
      outputStream =>
        guard.withPermit {
          IO(outputStream.close()).handleErrorWith(_ => IO.unit)
        }
    )

  def inputOutputStreams(in: File, out: File, guard: Semaphore[IO]): Resource[IO, (FileInputStream, FileOutputStream)] =
    for {
      inputStream <- inputStream(in, guard)
      outputStream <- outputStream(out, guard)
    } yield(inputStream, outputStream)

  def transmit(inputStream: FileInputStream, outputStream: FileOutputStream, buffer: Array[Byte], acc: Long, iter: Long): IO[Result] =
    for {
      amount <- IO(inputStream.read(buffer, 0, buffer.size))
      count <-
        if (amount > -1) {
          println(s"$amount transferred on the ${iter+1} loop. Buffer size was ${buffer.size} Buffer length ${buffer.length}")
          IO(outputStream.write(buffer, 0, amount)) >> transmit(inputStream, outputStream, buffer, acc + amount, iter+1)
        } else IO(Result(acc, iter))
    } yield(count)

  def transfer(inputStream: FileInputStream, outputStream: FileOutputStream): IO[Result] =
    for {
      buffer <- IO(new Array[Byte](1024*10))
      result <- transmit(inputStream: FileInputStream, outputStream: FileOutputStream, buffer, 0L, 0L)
    } yield(result)

  def copy(origin: File, destination: File)(implicit concurrent: Concurrent[IO]): IO[Result] = {
    for {
      guard <- Semaphore[IO](1)
      result <- inputOutputStreams(origin, destination, guard).use {
        case(in, out) => guard.withPermit(transfer(in, out))
      }
    } yield(result)
  }
  //implicit val ctx = IO.contextShift(ExecutionContext.global)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <-
        if (args.length < 2)
          IO.raiseError(new IllegalArgumentException("Need origin and destination files!"))
        else
          IO.unit
      result <- copy(new File(args(0)), new File(args(1)))
      _ <- IO(println(s"${result.count} bytes transferred from ${args(0)} to ${args(1)}. Looped ${result.iter} times."))
    } yield (ExitCode.Success)
}
