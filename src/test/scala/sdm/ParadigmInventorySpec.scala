package sdm

import org.scalatest.FunSuite

class ParadigmInventorySpec extends FunSuite {

  test("A ParadigmInventory should contain the right number of Paradigms for its cell count") {
    val lengths = (1 to 12) map { n =>
      val l = new ParadigmInventory(n).length
      println(f"$n%2d cells --> $l paradigms")
      l
    }
    assert(lengths === Seq(1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975, 678570, 4213597 /*, 27644437, 190899322*/ ))
  }

  test("A ParadigmInventory should implement indexOf() correctly") {
    val pi = new ParadigmInventory(6)
    val p0 = Paradigm(1,1,1,1,1,1)
    assert(pi.indexOf(p0) === 0)
    val p1 = Paradigm(1,1,1,1,1,2)
    assert(pi.indexOf(p1) === 1)
    val p3 = Paradigm(1,1,1,1,2,2)
    assert(pi.indexOf(p3) === 3)
    val p200 = Paradigm(1,2,3,4,5,4)
    assert(pi.indexOf(p200) === 200)
    val p201 = Paradigm(1,2,3,4,5,5)
    assert(pi.indexOf(p201) === 201)
    val p202 = Paradigm(1,2,3,4,5,6)
    assert(pi.indexOf(p202) === 202)
  }
  
}