object ValidationExceptions {

  sealed trait CipherException
  final case class KeyValidationException(error: String) extends AnyVal with CipherException
  final case class DataValidationException(error: String) extends AnyVal with CipherException
}