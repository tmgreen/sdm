package sdm

import org.scalatest.FunSuite

class ParadigmSpec extends FunSuite {

  test("Paradigm works in simplest case (1 cell)") {
    val p = Paradigm(1L)
    assert(p.length === 1)
    assert(p(0) === 1)
    assert(p.toSeq === IndexedSeq(1))
  }

  test("Paradigm works") {
    val p = Paradigm(1, 2, 3, 4, 2, 1, 1)
    assert(p.length === 7)
    assert(p(0) === 1)
    assert(p(3) === 4)
    assert(p(6) === 1)
    assert(p.toSeq === IndexedSeq(1, 2, 3, 4, 2, 1, 1))
  }

}