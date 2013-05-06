package sdm

import org.scalatest.FunSuite

class ParadigmSetSpec extends FunSuite {

  test("A ParadigmSet should be creatable from a ParadigmInventory") {
    val pi = new ParadigmInventory(6)
    val ps = new ParadigmSet(pi)
    ps ++= List(0,1,2,200,201,202)
    println(ps)
    assert(ps.contains(Paradigm(1,1,1,1,1,1)))
  }

  test("ParadigmSet addition should work") {
    val pi = new ParadigmInventory(6)
    val ps = new ParadigmSet(pi)
    ps ++= List(0,1,2,200,201,202)
    println(ps)
    val par = Paradigm(1,1,1,2,3,4)
    assert(!ps.contains(par))
    ps += par
    println(ps)
    assert(ps.contains(par))
  }

}