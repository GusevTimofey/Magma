package algorithm

import com.google.common.primitives.Ints
import scala.annotation.tailrec

final class Magma(
  expandedKey: List[Array[Byte]]
) {

  def t(
    bytes: Array[Byte]
  ): Int =
    Ints.fromByteArray((0 until 4).foldLeft(Array.emptyByteArray) {
      case (array: Array[Byte], position: Int) =>
        val init: Int             = (bytes(position) & 0xf0) >>> 4
        val last: Int             = bytes(position) & 0x0f
        val iterationResult: Byte = ((pi(position * 2 * 16 + init) << 4) | pi((position * 2 + 1) * 16 + last)).toByte
        array :+ iterationResult
    })

  def g(
    roundKey: Array[Byte],
    rightBytes: Array[Byte]
  ): Array[Byte] =
    (3 to 0 by -1)
      .foldLeft(0: Int, Array.emptyByteArray) {
        case ((state: Int, output: Array[Byte]), position: Int) =>
          val newState: Int = roundKey(position) + rightBytes(position) + (state >>> 8)
          newState -> (output :+ (newState & 0xff).toByte)
      }
      ._2
      .reverse

  def round(
    roundKey: Array[Byte],
    leftSide: Array[Byte],
    rightSide: Array[Byte],
    isLastRound: Boolean = false
  ): Array[Byte] = {
    val tResult: Int          = t(g(roundKey, rightSide))
    val nextStep: Array[Byte] = Ints.toByteArray((tResult << 11) | (tResult >>> 21))
    val roundResult: Array[Byte] = nextStep.zip(leftSide).foldLeft(Array.emptyByteArray) {
      case (bytes, (nextIteration, leftIteration)) =>
        bytes :+ (nextIteration ^ leftIteration).toByte
    }
    if (isLastRound) roundResult ++ rightSide
    else rightSide ++ roundResult
  }

  @tailrec
  def roundIteration(
    input: Array[Byte],
    key: List[Array[Byte]],
    roundNumber: Int = 0,
    isDecipher: Boolean = false
  ): Array[Byte] = {
    val leftSide: Array[Byte]  = input.take(4)
    val rightSide: Array[Byte] = input.drop(4)
    if (!isDecipher && roundNumber != key.length - 1)
      roundIteration(round(key(roundNumber), leftSide, rightSide), key, roundNumber + 1, isDecipher)
    else if (!isDecipher) round(key.last, leftSide, rightSide, isLastRound = true)
    else if (roundNumber != 0)
      roundIteration(round(key(roundNumber), leftSide, rightSide), key, roundNumber - 1, isDecipher)
    else round(key.head, leftSide, rightSide, isLastRound = true)
  }

  def encrypt(data: Array[Byte]): Array[Byte] =
    roundIteration(data, expandedKey)

  def decrypt(data: Array[Byte]): Array[Byte] =
    roundIteration(data, expandedKey, expandedKey.length - 1, isDecipher = true)

  private val pi: Array[Int] = Array(
    1, 7, 14, 13, 0, 5, 8, 3, 4, 15, 10, 6, 9, 12, 11, 2, 8, 14, 2, 5, 6, 9, 1, 12, 15, 4, 11, 0, 13, 10, 3, 7, 5, 13,
    15, 6, 9, 2, 12, 10, 11, 7, 8, 1, 4, 3, 14, 0, 7, 15, 5, 10, 8, 1, 6, 13, 0, 9, 3, 14, 11, 4, 2, 12, 12, 8, 2, 1,
    13, 4, 15, 6, 7, 0, 10, 5, 3, 14, 9, 11, 11, 3, 5, 8, 2, 15, 10, 13, 14, 1, 7, 4, 12, 9, 6, 0, 6, 8, 2, 3, 9, 10, 5,
    12, 1, 14, 4, 7, 11, 13, 0, 15, 12, 4, 6, 2, 10, 5, 11, 9, 14, 8, 13, 7, 0, 3, 15, 1
  )
}

object Magma {

  def expandKey(key: Array[Byte]): List[Array[Byte]] = {
    val iGroup: List[Array[Byte]] = key.grouped(4).toList
    (0 until 3).foldLeft(List.empty[Array[Byte]]) {
      case (acc: List[Array[Byte]], _) =>
        acc ++ iGroup
    } ++ iGroup.reverse
  }

  def apply(rawKey: Array[Byte]): Magma = new Magma(expandKey(rawKey))
}
