import zio._

object Main extends App {

  override def run(args: List[String]) =
    for {
      magma <- MagmaCipher.initializeScheme(64)
      keys  <- magma.initializeUserKey
      data  <- magma.getData
    } yield 1

}
