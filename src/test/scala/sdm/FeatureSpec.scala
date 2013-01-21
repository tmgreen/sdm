package sdm

import org.scalatest.FunSuite

class FeatureSpec extends FunSuite {

  test("A Feature should be creatable from a series of 0's and 1's") {
    val f = Feature(0, 1, 0, 0, 1, 1)
    assert(f(0) === 0)
    assert(f(1) === 1)
    assert(f(2) === 0)
    assert(f(3) === 0)
    assert(f(4) === 1)
    assert(f(5) === 1)
  }

  test("A Feature should be creatable from a mask") {
    val f = Feature.fromMask(6, 0x32)
    assert(f(0) === 0)
    assert(f(1) === 1)
    assert(f(2) === 0)
    assert(f(3) === 0)
    assert(f(4) === 1)
    assert(f(5) === 1)
  }

  test("Feature.apply() should throw IndexOutOfBoundsException if index given is out of range") {
    val f = Feature(0, 1, 0, 0, 1, 1)
    intercept[IndexOutOfBoundsException] {
      val illegal = f(6)
    }
  }

  test("Feature.bitsSet() should correctly count number of bits set") {
    val f = Feature(0, 1, 0, 0, 1, 1)
    assert(f.bitsSet === 3)
  }

  test("Feature.compare() should correctly order features") {
    val f1 = Feature(0, 1, 0, 0, 0, 1)
    val f2 = Feature(0, 1, 0, 1, 0, 1)
    val f3 = Feature(0, 0, 1, 1, 0, 1)
    val f2b = Feature(0, 1, 0, 1, 0, 1)
    assert(f1.compare(f2) < 0)
    assert(f1.compare(f3) < 0)
    assert(f2.compare(f3) < 0)
    assert(f2.compare(f2b) === 0)
  }

  test("Feature.equals() should return true for equivalent features") {
    val f1 = Feature(0, 1, 0, 0, 1, 1)
    val f2 = Feature.fromMask(6, 0x32)
    assert(f1 === f2)
  }

  test("Feature.equals() should return false for features with equal bitmask but different length") {
    val f1 = Feature.fromMask(7, 0x32)
    val f2 = Feature.fromMask(6, 0x32)
    assert(f1.mask === f2.mask)
    assert(f1 != f2)
  }

  test("Feature.equals() should return false for features with equal length but different bitmasks") {
    val f1 = Feature.fromMask(6, 0x33)
    val f2 = Feature.fromMask(6, 0x32)
    assert(f1.mask != f2.mask)
    assert(f1 != f2)
  }

}