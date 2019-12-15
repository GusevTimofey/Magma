import algorithm.Magma
import com.google.common.primitives.Ints
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MagmaTests extends AnyWordSpec with Matchers {

  "Magma impl" should {
    val rawKey = Array(
      0xffeeddcc, 0xbbaa9988, 0x77665544, 0x33221100, 0xf0f1f2f3, 0xf4f5f6f7, 0xf8f9fafb, 0xfcfdfeff
    )
    val dummyKey: Array[Byte] = rawKey.flatMap(Ints.toByteArray)
    val magmaImpl: Magma      = Magma.apply(dummyKey)

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
        expandedKeyToBytes(index) shouldBe expandedKeyPerfectResult(index)
      }
    }
    "provide correct t function" in {
      magmaImpl.t(Ints.toByteArray(0xfdb97531)) shouldBe 0x2a196f34
      magmaImpl.t(Ints.toByteArray(0x2a196f34)) shouldBe 0xebd9f03a
      magmaImpl.t(Ints.toByteArray(0xebd9f03a)) shouldBe 0xb039bb3d
      magmaImpl.t(Ints.toByteArray(0xb039bb3d)) shouldBe 0x68695433
    }
    "provide correct round function" in {
      val round1 = magmaImpl.round(
        Ints.toByteArray(0xffeeddcc),
        Ints.toByteArray(0xfedcba98),
        Ints.toByteArray(0x76543210)
      )
      round1.sameElements(Array(0x76543210, 0x28da3b14).flatMap(Ints.toByteArray)) shouldBe true

      val round20 = magmaImpl.round(
        Ints.toByteArray(0x33221100),
        Ints.toByteArray(0xe7116722),
        Ints.toByteArray(0x89cadf21)
      )
      round20.sameElements(Array(0x89cadf21, 0xbac8444d).flatMap(Ints.toByteArray)) shouldBe true

      val round32 = magmaImpl.round(
        Ints.toByteArray(0xffeeddcc),
        Ints.toByteArray(0x239a4577),
        Ints.toByteArray(0xc2d8ca3d),
        isLastRound = true
      )
      round32.sameElements(Array(0x4ee901e5, 0xc2d8ca3d).flatMap(Ints.toByteArray)) shouldBe true
    }
    "provide correct encrypt function" in {
      val input: Array[Int]      = Array(0xfedcba98, 0x76543210)
      val encrypted: Array[Byte] = magmaImpl.encrypt(input.flatMap(Ints.toByteArray))
      encrypted.sameElements(Array(0x4ee901e5, 0xc2d8ca3d).flatMap(Ints.toByteArray)) shouldBe true
    }
    "provide correct decrypt function" in {
      val input: Array[Int]      = Array(0x4ee901e5, 0xc2d8ca3d)
      val decrypted: Array[Byte] = magmaImpl.decrypt(input.flatMap(Ints.toByteArray))
      decrypted.sameElements(Array(0xfedcba98, 0x76543210).flatMap(Ints.toByteArray)) shouldBe true
    }
  }
}
