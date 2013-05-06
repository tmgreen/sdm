package sdm

import org.scalatest.FunSuite
import scala.collection.immutable.Stream

class FeatureInventorySpec extends FunSuite {

  test("A FeatureInventory should contain the right number of features for its cell count") {
    (2 to 9) foreach { n =>
      val fi = new FeatureInventory(n)
      // println(s"NCELLS = $n:")
      // println(fi)
      // println
      assert(fi.length === math.pow(2, n) - 1)
    }
  }

  test("A FeatureInventory should properly implement MatrixLike[Int]") {
    val fi = new FeatureInventory(4)
    // println(fi)
    assert(fi.cell(0, 0) === 0)
    assert(fi.cell(2, 0) === 0)
    assert(fi.cell(3, 0) === 1)
    assert(fi.cell(0, 14) === 1)
    assert(fi.cell(3, 14) === 1)
  }

  test("A FeatureInventory should stream all subsets of length 2") {
    val fi = new FeatureInventory(4)
    val fs2 = fi.featureSets(2).toList
    assert(fs2.length === 105)
    assert(fs2.toSet.size === 105)
    // fi.featureSets(2) foreach { fs => println(fs); println }
  }

  test("A FeatureInventory should stream all subsets of length 3") {
    val fi = new FeatureInventory(4)
    val fs3 = fi.featureSets(3).toList
    assert(fs3.length === 5 * 7 * 13)
    assert(fs3.toSet.size === 5 * 7 * 13)
    // fi.featureSets(3) foreach { fs => println(fs); println }
  }

  test("A FeatureInventory should stream all subsets of length 4") {
    val fi = new FeatureInventory(4)
    val fs4 = fi.featureSets(4).toList
    assert(fs4.length === 5 * 7 * 13 * 3)
    assert(fs4.toSet.size === 5 * 7 * 13 * 3)
    // fi.featureSets(4) foreach { fs => println(fs); println }
  }

  test("A FeatureInventory should stream a huge subset with no memory requirements") {
    var x = 0
    // this shows that ranges work fast with no mem
    for {
      i <- 1 to 255 - 3
      j <- i + 1 to 255 - 2
      k <- j + 1 to 255 - 1
      l <- k + 1 to 255
    } { x += 1 }
    println(s"X = $x")
    assert(x === (85 * 127 * 253 * 63))
    val fi = new FeatureInventory(8)
    var y: Long = 0L
    // Stream.from(1) take(1000000000) foreach { y |= _ }
    // but featureSets() still blows up with OOME
    fi.featureSets(2) foreach { z => y += 1 }
    assert(y === (127 * 255))
    y = 0L
    fi.featureSets(3) foreach { z => y += 1 }
    assert(y === (127 * 85 * 253))

    //y = 0L
    //fi.featureSets(4) foreach { z => y += 1 }
    //assert(y === (85L * 127 * 253 * 63))
  }

  test("Simple loop performance") {

    var t = System.currentTimeMillis()
    for {
      x <- 0 to 10000000
    } {
      if (x % 1000000 == 0) {
        var t2 = System.currentTimeMillis()
        println(x + ": " + (t2 - t))
      }
    }

  }

  test("Simple Stream performance") {

    var t = System.currentTimeMillis()
    Stream.range(0, 10000000) foreach { x =>
      if (x % 1000000 == 0) {
        var t2 = System.currentTimeMillis()
        println(x + ": " + (t2 - t))
      }
    }

  }

  test("Control case for Large streams should work 2") {
    import collection._

    var result = 0
    val t = System.currentTimeMillis()
    for {
      x <- 0 to 10
      y <- x + 1 to 100000000
    } {
      if (y == 100000000) {
        result ^= x
        println(x + ": " + result)
      }
    }
    val t2 = System.currentTimeMillis()
    println(s"Elapsed: ${t2 - t}")
  }

  test("Large streams should work 2") {
    import collection._
    def streamer1: Stream[(Int, Int)] = {
      (for {
        x <- 0 to 10
        y <- x + 1 to 100000000
      } yield (x, y))(breakOut)
    }

    def streamer2: Stream[(Int, Int)] = {
      def next(x: Int, y: Int): Stream[(Int, Int)] = {
        if (y < 100000000)
          (x, y + 1) #:: next(x, y + 1)
        else if (x < 10)
          (x + 1, x + 2) #:: next(x + 1, x + 2)
        else
          Stream.empty
      }

      next(0, 0)
    }

    val t = System.currentTimeMillis()
    var x = 0
    streamer2 foreach { p =>
      if (p._2 == 100000000) {
        x ^= p._1
        println(p._1 + ": " + x)
      }
    }
    val t2 = System.currentTimeMillis()
    println(s"Elapsed: ${t2 - t}")

  }

}
