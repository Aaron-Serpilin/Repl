package repls
import scala.collection.mutable

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "Base-repl"
    override val operationHandler: Map[String, (Base, Base) => Base] = Map(
        "+" -> ((a, b) => a + b),
        "-" -> ((a, b) => a - b),
        "*" -> ((a, b) => a * b)
    )
    override val emptyValue: Int = 0

    override def pushBaseValueToExpressionStack(outputStack: mutable.Stack[Expression], element: String): Unit = {
        outputStack.push(Const(element.toInt))
    }

    override def baseVariableAssigner (expression: Array[String], result: String, variableName: String): Unit = {
        variablesMap(variableName) = result.toInt
    }

    override def RPNToResult(expression: Seq[String]): Base = {
        val outputStack = mutable.Stack[Base]() // Change the type of outputStack
        expression.foreach {
            case token if isOperator(token) =>
                val firstOperand = outputStack.pop()
                val secondOperand = outputStack.pop()
                val result = applyOperation(secondOperand, token, firstOperand)
                outputStack.push(result)

            case token if isInteger(token) =>
                outputStack.push(token.toInt)

        }
        outputStack.head
    }

    override def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.map {
            case variable if variablesMap.contains(variable) =>
                variablesMap(variable).toString
            case other =>
                other
        }
    }
}
