package repls
import scala.collection.mutable

/*
    The parent class of IntREPL and MultiSetREPL.
 */
abstract class REPLBase extends REPL {

    type Base
    //def evaluate (expression: String): Base = _

    // Dictionaries to store variablesMap and their values for IntRepl and MultiSetRepl
    val intVariablesMap = mutable.Map[String, Int]()
    val multiSetVariablesMap = mutable.Map[String, MultiSet[String]]()
}
