package sdm

import org.scalatest.FunSuite
import scala.collection.immutable.Stream

class TheorySpec extends FunSuite {

  test("A Theory should contain the right number of features for its cell count") {
    (2 to 9) foreach { n =>
      val th = Theory(n)
      assert(th.totalFeatures === math.pow(2, n) - 1)
    }
  }

  test("A FeatureSet should be creatable from a Theory") {
    val th = Theory(5)
    val fs = th.featureSet(1, 2, 31)
    assert(fs(0) === Feature("00001"))
    assert(fs(1) === Feature("00010"))
    assert(fs(2) === Feature("11111"))
  }

}
