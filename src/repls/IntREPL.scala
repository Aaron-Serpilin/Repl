package repls

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "int-repl"

    override def readEval(command: String): String = {
        val elements = command.split(" ")
        val result = elements.mkString(" ")
        result
    }


}
