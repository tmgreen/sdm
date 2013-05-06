package sdm

import org.scalatest.FunSuite

class FeatureSpec extends FunSuite {

  test("A Feature should be creatable from a mask") {
    val f = Feature(6, 0x32) // 110010
    assert(f.cell(0) === 1)
    assert(f.cell(1) === 1)
    assert(f.cell(2) === 0)
    assert(f.cell(3) === 0)
    assert(f.cell(4) === 1)
    assert(f.cell(5) === 0)
    assert(f.asInt === 50)
    assert(f.toBitString === "110010")
    println(f)
  }

  test("A Feature should be creatable from a series of 0's and 1's") {
    val f = Feature.fromBits(0, 1, 0, 0, 1, 1)
    assert(f.cell(0) === 0)
    assert(f.cell(1) === 1)
    assert(f.cell(2) === 0)
    assert(f.cell(3) === 0)
    assert(f.cell(4) === 1)
    assert(f.cell(5) === 1)
  }

  test("A Feature should be creatable from a string") {
    val f = Feature("01001")
    assert(f.cell(0) === 0)
    assert(f.cell(1) === 1)
    assert(f.cell(2) === 0)
    assert(f.cell(3) === 0)
    assert(f.cell(4) === 1)
    assert(f.toBitString === "01001")
    assert(f.asInt === 9)
  }

  test("A Feature should allow 31 cells") {
    val f = Feature("1011001110001111000011111000011")
    assert(f.length === 31)
    assert(f.cell(0) === 1)
    assert(f.cell(1) === 0)
    assert(f.cell(28) === 0)
    assert(f.cell(29) === 1)
    assert(f.cell(30) === 1)
    assert(f.mask === 0x59c787c3)
    assert(f.asInt === 0x59c787c3)
    assert(f.toBitString === "1011001110001111000011111000011")
    println(f)
  }

  test("A Feature should not allow 32 cells") {
    intercept[IndexOutOfBoundsException] {
      Feature("10110011100011110000111110000011")
    }
  }

}