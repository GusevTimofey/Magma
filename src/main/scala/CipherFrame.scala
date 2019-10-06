trait CipherFrame[T] {

  def encrypt(block: T): Array[Byte]

  def decrypt(block: Array[Byte]): T

  def keyExpansion(initialKey: T): Array[Byte]

  def blockIteration(activity: T => Array[Byte]): Array[Byte]
}