package sdm

import scala.collection.IndexedSeq
import scala.collection.mutable.BitSet

/** MUTABLE class for tallying paradigms
  */
class ParadigmSet (val inventory: ParadigmInventory) {

  private[ParadigmSet] var index: BitSet = BitSet.empty
    
  def contains(par: Paradigm) = index.contains(inventory.indexOf(par))

  def containsIndex(i: Int) = index.contains(i)

  /** Add a new paradigm to the set as efficiently as possible.
    */
  def add(par: Paradigm): Boolean = index add inventory.indexOf(par)

  def +=(par: Paradigm): ParadigmSet = { add(par); this }

  def ++=(indexes: TraversableOnce[Int]): ParadigmSet = { index ++= indexes; this }

  def ++=(otherSet: ParadigmSet): ParadigmSet = { index = index | otherSet.index; this }

  def iterator: Iterator[Paradigm] = new Iterator[Paradigm] {
    private[this] val iter = index.iterator
    override def hasNext = iter.hasNext
    override def next = inventory(iter.next)
  }
  
  override def toString = {
    index.toIndexedSeq map {i => inventory(i)} mkString(",")
  }
  
}


