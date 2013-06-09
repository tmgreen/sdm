package sdm

import org.scalatest.FunSuite
import scala.collection.immutable.Stream

class FeatureInventorySpec extends FunSuite {

  test("A FeatureInventory should properly implement MatrixLike[Int]") {
    implicit val th = Theory(4)
    val fi = new FeatureInventory()
    // println(fi)
    assert(fi.cell(0, 0) === 0)
    assert(fi.cell(2, 0) === 0)
    assert(fi.cell(3, 0) === 1)
    assert(fi.cell(0, 14) === 1)
    assert(fi.cell(3, 14) === 1)
  }

}
