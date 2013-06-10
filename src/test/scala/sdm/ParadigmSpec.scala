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

  test("Paradigm works in 8-cell case") {
    val p = Paradigm(1, 1, 1, 1, 1, 1, 1, 1)
    assert(p.length === 8)
    assert(p(0) === 1)
    assert(p(3) === 1)
    assert(p(6) === 1)
    assert(p(7) === 1)
    assert(p.toSeq === IndexedSeq(1, 1, 1, 1, 1, 1, 1, 1))
  }
  
  test("Paradigm.updated should work") {
    val p = Paradigm(1, 2, 3, 4, 2, 4, 1)
    println(p)
    assert(p(5) === 4)
    val p2 = p.updated(5, 1)
    println(p2)
    assert(p2(5) === 1)
    assert(p2.toString === "{1,2,3,4,2,1,1}")
  }

  
  test("Paradigm.updated should work for ncells > 8") {
    val p = Paradigm(1, 2, 3, 4, 2, 4, 1, 5, 6)
    println(p)
    assert(p(8) === 6)
    assert(p.toString === "{1,2,3,4,2,4,1,5,6}")
    val p2 = p.updated(8, 5)
    println(p2)
    assert(p2(8) === 5)
    assert(p2.toString === "{1,2,3,4,2,4,1,5,5}")
  }

  
  
}