package repls

import repls.MultiSet.empty

/*
    Multiset is a Map of elements and their respective count.
    For example:
    {a,a,a,b,c,c} = Map('a'->3, 'b'->1, 'c'->2)
 */


case class MultiSet[T] (multiplicity: Map[T, Int]) {

    // Intersection of multi sets
    def *(that: MultiSet[T]): MultiSet[T] = {
        val intersection = this.multiplicity.map {
            case (elem, count) => elem -> math.min(count, that.multiplicity.getOrElse(elem, 0)) // We only return the elements in the this and that multi sets that have a count greater than 0
        }
        MultiSet(intersection.filter{case (_, count) => count > 0})
    }

    // Addition of multi sets
    def +(that: MultiSet[T]): MultiSet[T] = {

        val summation = this.multiplicity.map {
            case (elem, count) => elem -> (count + that.multiplicity.getOrElse(elem, 0)) // We add the values of the multi sets, and if there is no value, we fill with a 0
        } ++ (that.multiplicity -- this.multiplicity.keys)
        MultiSet(summation.filter{case (_, count) => count > 0})

    }

    // Subtraction of multi sets
    def -(that: MultiSet[T]): MultiSet[T] = {
        val subtraction = this.multiplicity.map {
            case (elem, count) => elem -> math.max(count - that.multiplicity.getOrElse(elem, 0), 0) // We subtract the values of the multi sets, and if there is no value, we fill with a 0
        }
        MultiSet(subtraction.filter{case (_, count) => count > 0})
    }

    def toSeq: Seq[T] = {
      multiplicity.flatMap { case (elem, count)=> Seq.fill(count)(elem) }.toSeq
    }

  val MaxCountForDuplicatePrint = 5

    override def toString: String = {
        def elemToString(elem : T) : String = {
            val count = multiplicity(elem)
            if(count >= MaxCountForDuplicatePrint)
                elem.toString + " -> " + count.toString
            else Seq.fill(count)(elem).mkString(",")
        }
        val keyStringSet = multiplicity.keySet.map(elemToString)
        "{" + keyStringSet.toSeq.sorted.mkString(",") + "}"
    }
}

object MultiSet {
    def empty[T]: MultiSet[T] = MultiSet(Map[T, Int]())

    // Constructor to create a multiset from a sequence of elements
    def apply[T](elements: Seq[T]): MultiSet[T] = {
        val multiSetMap = elements.groupBy(identity).view.mapValues(_.size).toMap // We group elements by their keys, then associate the number of elements there are, and then map it out
        new MultiSet(multiSetMap)
    }
}
