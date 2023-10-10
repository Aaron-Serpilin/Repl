package repls

import repls.MultiSet.empty

/*
    Multiset is a Map of elements and their respective count.
    For example:
    {a,a,a,b,c,c} = Map('a'->3, 'b'->1, 'c'->2)
 */


case class MultiSet[T] (multiplicity: Map[T, Int]) {

    /* TODO
        Intersection of two multisets:
        ∀x m_c(x) = min(m_a(x), m_b(x))
        Example:
        {a,b,b,c,c,c} * {b,c,c,c,c} = {b,c,c,c}
     */
    def *(that: MultiSet[T]): MultiSet[T] = empty[T]

    /* TODO
        Summation of two multisets:
        ∀x m_c(x) = m_a(x) + m_b(x)
        Example:
        {a,b,c,c} + {a,c,d} = {a,a,b,c,c,c,d}
     */
    def +(that: MultiSet[T]): MultiSet[T] = empty[T]

    /* TODO
        Subtraction of two multisets:
        ∀x m_c(x) = max(m_a(x) - m_b(x), 0)
        Example:
        {a,b,b,d} - {b,c,c,d,d} = {a,b}
     */
    def -(that: MultiSet[T]): MultiSet[T] = empty[T]
    /* TODO
        Make sure a multiset can be returned as a sequence.

        For example the multiset {a,a,b} should give the sequence Seq(a,a,b).

        The order of the elements in the sequence does not matter.
     */
    def toSeq: Seq[T] = {
        Seq.empty
    }

    val MaxCountForDuplicatePrint = 5

    // A toString has already been provided
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
    def empty[T] : MultiSet[T] = MultiSet(Map[T,Int]())
    /* TODO
        Write a constructor that constructs a multiset from a sequence of elements
     */
    def apply[T](elements: Seq[T]): MultiSet[T] = empty[T]
}
