package sdm

import org.scalatest.FunSuite

class ParadigmInventorySpec extends FunSuite {

  test("A ParadigmInventory should contain the right number of features for its cell count") {
    val lengths = (1 to 12) map { n =>
      val l = new ParadigmInventory(n).length
      println(f"$n%2d --> $l")
      l
    }
    assert(lengths === Seq(1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597 /*, 27644437, 190899322*/ ))
  }

}