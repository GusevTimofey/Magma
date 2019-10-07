import zio._

object Main extends App {

  override def run(args: List[String]): ZIO[Main.Environment, Nothing, Int] =
    (for {
      magma         <- MagmaCipher.initializeScheme(64)
      keys          <- magma.initializeUserKey
      data          <- magma.initializeData
      dividedData   <- magma.expandData(data.getBytes)
      encryptedData <- magma.encrypt(keys, dividedData)
      decryptedData <- magma.decrypt(keys, encryptedData)
    } yield 0) orElse ZIO.succeed(1)

}
