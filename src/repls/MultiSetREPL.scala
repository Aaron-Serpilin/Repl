package repls
import scala.collection.mutable
import repls.MultiSet.empty

class MultiSetREPL extends REPLBase {

    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"
    override val operationHandler: Map[String, (Base, Base) => Base] = Map(
        "+" -> ((a, b) => a + b),
        "-" -> ((a, b) => a - b),
        "*" -> ((a, b) => a * b)
    )
    override val emptyValue: MultiSet[String] = empty[String]

    override def pushBaseValueToExpressionStack(outputStack: mutable.Stack[Expression], element: String): Unit = {
        val elementAsSeq = Seq(element) // Convert to a sequence
        outputStack.push(Const(MultiSet(elementAsSeq)))
    }

    def baseVariableAssigner (expression: Array[String], result: String, variableName: String): Unit = {
        variablesMap(variableName) = MultiSet(expression)
    }

    override def RPNToResult(expression: Seq[String]): Base = {
        val outputStack = mutable.Stack[Base]()
        expression.foreach {
            case token if isOperator(token) =>
                val firstOperand = outputStack.pop()
                val secondOperand = outputStack.pop()
                val result = applyOperation(secondOperand, token, firstOperand)
                outputStack.push(result)

            case token if isVariable(token) =>
                val variableValue = variablesMap.getOrElse(token, MultiSet.empty)
                outputStack.push(variableValue)

            case token if token.length > 1 => // Handle multi sets
                val multiSetElements = token.drop(1).dropRight(1).split(",").toSeq
                val multiSet = MultiSet(multiSetElements)
                outputStack.push(multiSet)
        }
        outputStack.head
    }

    override def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.flatMap {
            case variable if variablesMap.contains(variable) =>
                val variableValue = variablesMap(variable)
                variableValue.toSeq.map(_.toString)

            case other =>
                Seq(other)
        }
    }
}