package sdm

import org.scalatest.FunSuite

class FeatureSpec extends FunSuite {

  test("A Feature should be creatable from a mask") {
    val f = Feature(0x32) // 110010
    assert(f.nthHighestBit(0, 6) === 1)
    assert(f.nthHighestBit(1, 6) === 1)
    assert(f.nthHighestBit(2, 6) === 0)
    assert(f.nthHighestBit(3, 6) === 0)
    assert(f.nthHighestBit(4, 6) === 1)
    assert(f.nthHighestBit(5, 6) === 0)
    assert(f.asInt === 50)
    assert(f.toBitString(6) === "110010")
    println(f)
  }

  test("A Feature should be creatable from a series of 0's and 1's") {
    val f = Feature.fromBits(0, 1, 0, 0, 1, 1)
    assert(f.toBitString(6) === "010011")
  }

  test("A Feature should be creatable from a string") {
    val f = Feature("01001")
    assert(f.nthBit(4) === 0)
    assert(f.nthBit(3) === 1)
    assert(f.nthBit(2) === 0)
    assert(f.nthBit(1) === 0)
    assert(f.nthBit(0) === 1)
    assert(f.toBitString(5) === "01001")
    assert(f.asInt === 9)
  }

  test("A Feature should allow 31 cells") {
    val f = Feature("1011001110001111000011111000011")
    assert(f.nthBit(30) === 1)
    assert(f.nthBit(29) === 0)
    assert(f.nthBit(2) === 0)
    assert(f.nthBit(1) === 1)
    assert(f.nthBit(0) === 1)
    assert(f.mask === 0x59c787c3)
    assert(f.asInt === 0x59c787c3)
    assert(f.toBitString(31) === "1011001110001111000011111000011")
    println(f)
  }

  test("A Feature should not allow 32 cells") {
    intercept[IndexOutOfBoundsException] {
      Feature("10110011100011110000111110000011")
    }
  }

}