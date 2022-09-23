import cats.effect
import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxFlatMapOps
import cats.effect._

import java.io.{File, FileInputStream, FileOutputStream}
import scala.concurrent.ExecutionContext
import scala.util.Try

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

  def transfer(inputStream: FileInputStream, outputStream: FileOutputStream, buffer: Int): IO[Result] =
    for {
      buffer <- IO(new Array[Byte](buffer*10))
      result <- transmit(inputStream: FileInputStream, outputStream: FileOutputStream, buffer, 0L, 0L)
    } yield(result)

  def copy(origin: File, destination: File, buffer: Int)(implicit concurrent: Concurrent[IO]): IO[Result] = {
    for {
      guard <- Semaphore[IO](1)
      result <- inputOutputStreams(origin, destination, guard).use {
        case(in, out) => guard.withPermit(transfer(in, out, buffer))
      }
    } yield(result)
  }
  //implicit val ctx = IO.contextShift(ExecutionContext.global)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO (println(s"src ${args(0)} dst ${args(1)} buffer ${args(2)}"))
      _ <- args match {
        case a if a.length < 2                            => IO.raiseError(new IllegalArgumentException("Need origin and destination files!"))
        case b if b(0) == b(1)                            => IO.raiseError(new Exception("Source Destination paths cannot be the same!"))
        case c if ! c(2).forall(Character.isDigit(_))     => IO.raiseError(new Exception("Buffer size has to be a numerical"))
        case _                                            => IO.unit
      }
      source <- IO(new File(args(0)))
      destination <-  IO {
        val dest = new File(args(1))
        dest.exists() match {
          case false =>
            dest
          case true  =>
            println(s"overwriting ${args(1)}. press y or n ?")
            val decision = scala.io.StdIn.readLine
            if (decision.strip().toLowerCase().contains('n')) System.exit(-1)
            dest
        }
      }
      result <- copy(source, destination, args(3).toInt)
      _ <- IO(println(s"${result.count} bytes transferred from ${args(0)} to ${args(1)}. Looped ${result.iter} times."))
    } yield (ExitCode.Success)
}
