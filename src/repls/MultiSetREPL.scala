package repls

class MultiSetREPL extends REPLBase {
    // Have a REPL of a MutliSet of strings
    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"

    override def readEval(command: String): String = {
        val tokens = command.split(" ").toList
        println(command)
        command
    }

    // TODO: Implement any further functions that are specifically for an MultiSetREPL
}
