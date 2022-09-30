import cats.Monad
import cats.data.State
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
//import cats.syntax.show

//State.
class Worker[F[_]](number: Int, ref: Ref[F, Int])(implicit F: Sync[F]){
  private def putStrLn(value: String): F[Unit] = F.delay(println(value))
  def start: F[Unit] =
    for {
      c1 <- ref.get
      _  <- putStrLn(s"before#$number >> $c1")
      //_  <- ref.update(x => x+1)
      //c2 <- ref.get
      c2 <- ref.modify(x => (x+1, x.toString+" hello"))
      c3 <- ref.get
      _  <- putStrLn(s"afters#$number >> $c2 & ${c3.getClass} $c3")
    } yield ()
}
object RefStudy extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      ref <- Ref.of[IO, Int](0)
      w1 = new Worker[IO](1, ref)
      w2 = new Worker[IO](2, ref)
      w3 = new Worker[IO](3, ref)
      _  <- List(w1.start, w2.start, w3.start).parSequence.void
    } yield(ExitCode.Success)
  }
}
