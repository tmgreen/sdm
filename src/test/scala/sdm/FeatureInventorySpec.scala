package sdm

import org.scalatest.FunSuite

class FeatureInventorySpec extends FunSuite {

  test("A FeatureInventory should contain the right number of features for its cell count") {
    (2 to 9) foreach { n =>
      assert(new FeatureInventory(n).length === math.pow(2, n) - 1)
    }
  }

  test("A FeatureInventory should have features in the right order") {
    val fi = new FeatureInventory(4)
    assert(fi(0) === Feature(1, 0, 0, 0))
    assert(fi(2) === Feature(0, 0, 1, 0))
    assert(fi(4) === Feature(1, 1, 0, 0))
    assert(fi(5) === Feature(1, 0, 1, 0))
    assert(fi(14) === Feature(1, 1, 1, 1))
    assert(fi(13) === Feature(0, 1, 1, 1))
  }

  test("A FeatureInventory should be queryable by feature bitmask") {
    val fi = new FeatureInventory(4)
    assert(fi(4) eq fi.featureForMask(0x03))
    assert(fi(5) eq fi.featureForMask(0x05))
  }
}
