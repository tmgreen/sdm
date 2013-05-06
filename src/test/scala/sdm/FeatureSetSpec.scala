package sdm

import org.scalatest.FunSuite

class FeatureSetSpec extends FunSuite {

  test("A FeatureSet should be creatable from a FeatureInventory") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 2, 31)
    assert(fs(0) === Feature("00001"))
    assert(fs(1) === Feature("00010"))
    assert(fs(2) === Feature("11111"))
  }

  test("A FeatureSet should yield F* with the andComplete() method") {
    val fs = FeatureSet(
      Feature("00001"),
      Feature("10110"),
      Feature("11101"))
    assert(fs.length === 3)
    val fstar = fs.andComplete
    assert(fstar.length === 4)
    assert(fstar contains Feature("10100"))
  }

  test("A FeatureSet should compute correct rowMasks 1") {
    val fs = FeatureSet(
      Feature("01"),
      Feature("10"))
    println(fs)
    // fs should be:
    // 0  1  
    // 1  0  

    // rowMasks is private and not predictable
    // assert(fs.rowMasks === Array[Int](1, 2))
    assert(fs.isFine === true)
  }

  test("FeatureSet addition should work") {
    val fi = new FeatureInventory(2)
    val fs = fi.featureSet(1)
    println(fs)
    // fs should be:
    // 0
    // 1

    // rowMasks is private and not predictable
    // assert(fs.rowMasks === Array[Int](0, 1))
    assert(fs.isComplete === false)
    assert(fs.isFine === false)

    val fs2 = fs + fi.feature(2)
    println(fs2)
    // fs2 should be:
    // 0  1  
    // 1  0  

    //assert(fs2.rowMasks === Array[Int](1, 2))
    assert(fs2.isComplete === true)
    assert(fs2.isFine === true)
  }

  test("A FeatureSet should compute correct rowMasks 2") {
    val fs = FeatureSet(
      Feature("01000"),
      Feature("01111"),
      Feature("10011"))

    println(fs)

    // fs should be:
    // 0  0  1  
    // 1  1  0  
    // 0  1  0  
    // 0  1  1  
    // 0  1  1  

    // rowMasks is private and not predictable
    // assert(fs.rowMasks === Array[Int](1, 6, 2, 3, 3))
    assert(fs.isFine === false)
  }

  test("A FeatureSet should compute correct finestParadigm") {
    val fs = FeatureSet(
      Feature("01000"),
      Feature("01111"),
      Feature("10011"))

    println(fs)

    // fs should be:
    // 0  0  1  
    // 1  1  0  
    // 0  1  0  
    // 0  1  1  
    // 0  1  1  

    assert(fs.finestParadigm === Paradigm(1, 2, 3, 4, 4))
  }

  test("A FeatureSet should support immutable addition with the + operator") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29) // 00001, 10110, 11101
    println(fs)
    val fs2 = fs + Feature("11100")
    assert(fs2 contains Feature("11100"))
    println(fs2)
    // fs2 is:
    // 0  1  1  1
    // 0  0  1  1
    // 0  1  1  1
    // 0  1  0  0
    // 1  0  0  1
    assert(fs2(2) === Feature("11100"))
  }

  test("A FeatureSet should support incremental F* computation with the addAndComplete() method") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29) // 00001, 10110, 11101
    val fstar = fs.andComplete
    // fstar is:
    // 0  1  1  1
    // 0  0  0  1
    // 0  1  1  1
    // 0  0  1  0
    // 1  0  0  1
    val fstar2 = fstar.addAndComplete(Feature("01110"))
    // fstar2 is:
    // 0  0  0  0  0  1  1  1
    // 0  0  0  1  1  0  0  1
    // 0  1  1  1  1  1  1  1
    // 0  0  1  0  1  0  1  0
    // 1  0  0  0  0  0  0  1
    assert(fstar2 contains Feature("00110"))
    assert(fstar2(2) === Feature("00110"))
    assert(fstar2.length === 8)
  }

  test("A FeatureSet should produce its subsets") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29) // 00001, 10110, 11101
    val subs = fs.subsets.toList
    // subs foreach { fs => println(fs); println }
    // println("-------------")
    assert(subs.length === 8)
    val compsubs = subs filter { _.isComplete }
    // compsubs foreach { fs => println(fs); println }
    // println("-------------")
    val pars = fs.allParadigms
    assert(pars === Set(Paradigm(1, 2, 1, 3, 2).mask, Paradigm(1, 2, 1, 3, 4).mask))
    // pars foreach { p => println(Paradigm(p)); println }
  }

}