object ValidationExceptions {

  import java.io.IOException

  sealed trait CipherException
  final case class KeyValidationException(error: String) extends IOException with CipherException
  final case class DataValidationException(error: String) extends IOException with CipherException
}