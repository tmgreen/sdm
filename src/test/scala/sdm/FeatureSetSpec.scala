package sdm

import org.scalatest.FunSuite

class FeatureSetSpec extends FunSuite {

  test("A FeatureSet should be creatable from a FeatureInventory with indexes") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 2, 30)
    assert(fs(0) === Feature(0, 1, 0, 0, 0))
    assert(fs(1) === Feature(0, 0, 1, 0, 0))
    assert(fs(2) === Feature(1, 1, 1, 1, 1))
  }

  test("A FeatureSet should yield F* with the andComplete() method") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29)
    assert(fs(0) === Feature(0, 1, 0, 0, 0))
    assert(fs(1) === Feature(1, 0, 0, 1, 1))
    assert(fs(2) === Feature(0, 1, 1, 1, 1))
    val fstar = fs.andComplete
    assert(fstar contains Feature(0, 0, 0, 1, 1))
    assert(fstar(1) === Feature(0, 0, 0, 1, 1))
  }

  test("A FeatureSet should compute correct rowMasks") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29)
    // fs2 is:
    // 0  1  0
    // 1  0  1
    // 0  0  1
    // 0  1  1
    // 0  1  1
    assert(fs.rowMasks === Array[Int](2, 5, 4, 6, 6))
  }

  test("A FeatureSet should compute correct finestParadigm") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29)
    // fs2 is:
    // 0  1  0
    // 1  0  1
    // 0  0  1
    // 0  1  1
    // 0  1  1
    assert(fs.finestParadigm === Paradigm(1, 2, 3, 4, 4))
  }

  test("A FeatureSet should support immutable addition with the +() method") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29)
    val fs2 = fs + Feature(1, 1, 1, 0, 0)
    assert(fs2 contains Feature(1, 1, 1, 0, 0))
    // fs2 is:
    // 0  1  1  0
    // 1  1  0  1
    // 0  1  0  1
    // 0  0  1  1
    // 0  0  1  1
    assert(fs2(1) === Feature(1, 1, 1, 0, 0))
  }

  test("A FeatureSet should support incremental F* computation with the addAndComplete() method") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29)
    val fstar = fs.andComplete
    val fstar2 = fstar.addAndComplete(Feature(1, 1, 1, 0, 0))
    assert(fstar2 contains Feature(0, 1, 1, 0, 0))
    assert(fstar2(2) === Feature(0, 1, 1, 0, 0))
  }

  test("A FeatureSet should produce its subsets") {
    val fi = new FeatureInventory(5)
    val fs = fi.featureSet(1, 22, 29)
    val subs = fs.subsets.toList
    subs foreach {fs => println(fs); println } 
    println("-------------")
    assert(subs.length === 8)
    val compsubs = subs filter { _.isComprehensive }
    compsubs foreach {fs => println(fs); println } 
    println("-------------")
    val pars = fs.allParadigms foreach { p => println(Paradigm(p)); println }
  }

}