import algorithm.Magma
import com.google.common.primitives.Ints
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MagmaTests extends AnyWordSpec with Matchers {

  "Magma impl" should {
    val rawKey = Array(0xffeeddcc, 0xbbaa9988, 0x77665544, 0x33221100, 0xf0f1f2f3, 0xf4f5f6f7, 0xf8f9fafb, 0xfcfdfeff)
    val dummyKey: Array[Byte] = rawKey.flatMap(Ints.toByteArray)
    val magmaImpl: Magma = Magma.apply(dummyKey)

    "provide correct t function" in {
      magmaImpl.t(Ints.toByteArray(0xfdb97531)) shouldBe 0x2a196f34
      magmaImpl.t(Ints.toByteArray(0x2a196f34)) shouldBe 0xebd9f03a
      magmaImpl.t(Ints.toByteArray(0xebd9f03a)) shouldBe 0xb039bb3d
      magmaImpl.t(Ints.toByteArray(0xb039bb3d)) shouldBe 0x68695433
    }
    "provide correct expandKey function" in {

      val expandedKeyPerfectResult: Array[Int] = Array(
        rawKey,
        rawKey,
        rawKey,
        rawKey.reverse
      ).flatten

      val expandedKey = Magma.expandKey(dummyKey)
      expandedKey.size shouldBe 32
      val expandedKeyToBytes = expandedKey.map(Ints.fromByteArray)
      (0 until 32).foreach { index: Int =>
        expandedKeyToBytes(index) shouldBe (expandedKeyPerfectResult(index))
      }
    }
    "provide correct cipher function" in {
      val inputData = "qwertyuiopasdfghjklzxcvbnmqwerty".getBytes()
      val decrypted = magmaImpl.encrypt(inputData)
      val encrypted = magmaImpl.decrypt(decrypted)
      decrypted.sameElements(encrypted)
    }
  }
}
