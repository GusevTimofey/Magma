import java.io.{ File, FileInputStream, IOException }
import java.nio.charset.StandardCharsets

import ValidationExceptions.{ DataValidationException, KeyValidationException }
import com.google.common.primitives.Ints
import zio.console.{ getStrLn, putStrLn, Console }
import zio.{ IO, Task, UIO, ZIO }
import zio.blocking._

import scala.language.postfixOps

final case class MagmaCipher(blockSize: Int) {

  def initializeUserKey: ZIO[Console, IOException, List[Array[Byte]]] =
    if (blockSize == 64) createKeys
    else ZIO.fail[IOException](KeyValidationException(s"Mod with $blockSize block size is not implemented now."))

  def initializeData: ZIO[Console, Throwable, String] =
    for {
      _        <- putStrLn(s"Enter file path to encrypt.")
      fileName <- getStrLn
      data     <- readData(fileName)
    } yield data

  def encrypt(roundKeys: List[Array[Byte]], data: Seq[Array[Byte]]): ZIO[Any, Throwable, List[Array[Byte]]] =
    for {
      result <- IO.collectAllPar(data.map(roundIterations(roundKeys, _)))
    } yield result

  def decrypt(roundKeys: List[Array[Byte]], data: Seq[Array[Byte]]): ZIO[Any, Throwable, List[Array[Byte]]] =
    for {
      result <- IO.collectAllPar(data.map(roundIterations(roundKeys, _)))
    } yield result

  def expandData(data: Array[Byte]): ZIO[Any, IOException, Seq[Array[Byte]]] =
    if (data.length % 64 == 0) ZIO.succeed(data.grouped(64).toSeq)
    else ZIO.fail[IOException](DataValidationException(s"data.length % 64 != 0. Current mod is not implemented yet."))

  private def roundIterations(roundKeys: List[Array[Byte]], data: Array[Byte]): Task[Array[Byte]] = IO {
//    val (leftPart: Array[Byte], rightPart: Array[Byte]) = data.head splitAt 32
//    (data zip roundKeys).foldLeft(Array.emptyByteArray, Array.emptyByteArray) {
//      case ((resultedBlock, _), (key, block)) =>
//        val iterationResult = roundEncryption(leftPart, rightPart, key)
    Array.emptyByteArray
  }

  private def roundEncryption(leftPart: Array[Byte],
                              rightPart: Array[Byte],
                              roundKey: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val fResult: Array[Byte]               = f(leftPart, roundKey)
    val replacementPhase: Seq[Array[Byte]] = (fResult grouped 2).toSeq
    (replacementPhase zip pi).flatMap {
      case (elems, sBoxes) =>
        elems.map(sBoxes(_))
    }.toArray[Byte] -> rightPart
  }

  private def f(bytes: Array[Byte], key: Array[Byte]): Array[Byte] =
    (bytes zip key) flatMap { case (ki, xi) => Ints.toByteArray((ki + xi) >>> 32) }

  private def createKeys: ZIO[Console, IOException, List[Array[Byte]]] =
    for {
      _   <- putStrLn("Enter your key:\n$>")
      key <- getStrLn
      _ <- validateKey(key).catchAll { error =>
            putStrLn(s"${error.toString}\n")
            createKeys
          }
      expandedKey <- initializeRoundKeys(key)
    } yield expandedKey

  private def initializeRoundKeys(initialKey: String): UIO[List[Array[Byte]]] =
    ZIO.succeed {
      val iGroup: Seq[Array[Byte]] = initialKey.getBytes.grouped(4).toSeq
      ((0 until 4).map(iGroup) ++ iGroup.reverse).toList
    }

  private def validateKey(key: String): ZIO[Any, KeyValidationException, String] =
    for {
      result <- ZIO.fromEither(
                 Either.cond(key.length / 8 == 4,
                             key,
                             KeyValidationException(
                               s"Error has occurred while user key validation. Key length is invalid: ${key.length}."
                             ))
               )
    } yield result

  private def closeStream(is: FileInputStream) =
    UIO(is.close())

  private def convertBytes(is: FileInputStream, len: Long): Task[String] =
    Task.effect(new String(is.readAllBytes(), StandardCharsets.UTF_8))

  private def readData(fileName: String): Task[String] =
    for {
      file   <- Task(new File(fileName))
      len    = file.length
      string <- Task(new FileInputStream(file)).bracket(closeStream)(convertBytes(_, len))
    } yield string

  private val pi: List[Array[Byte]] = List(
    Array(1, 7, 14, 13, 0, 5, 8, 3, 4, 15, 10, 6, 9, 12, 11, 2),
    Array(8, 14, 2, 5, 6, 9, 1, 12, 15, 4, 11, 0, 13, 10, 3, 7),
    Array(5, 13, 15, 6, 9, 2, 12, 10, 11, 7, 8, 1, 4, 3, 14, 0),
    Array(7, 15, 5, 10, 8, 1, 6, 13, 0, 9, 3, 14, 11, 4, 2, 12),
    Array(12, 8, 2, 1, 13, 4, 15, 6, 7, 0, 10, 5, 3, 14, 9, 11),
    Array(11, 3, 5, 8, 2, 15, 10, 13, 14, 1, 7, 4, 12, 9, 6, 0),
    Array(6, 8, 2, 3, 9, 10, 5, 12, 1, 14, 4, 7, 11, 13, 0, 15),
    Array(12, 4, 6, 2, 10, 5, 11, 9, 14, 8, 13, 7, 0, 3, 15, 1)
  )
}

object MagmaCipher {
  def initializeScheme(iterationsNumber: Int): ZIO[Any, Nothing, MagmaCipher] =
    ZIO.succeed(MagmaCipher(iterationsNumber))
}
