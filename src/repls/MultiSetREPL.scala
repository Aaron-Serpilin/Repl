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

    override def pushBaseValueToBaseStack(outputStack: mutable.Stack[MultiSet[String]], element: String): Unit = {
    val elementAsSeq = Seq(element)
    outputStack.push(MultiSet(elementAsSeq))
    }

    private def RPNToResult(expression: Seq[String]): Base = {
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

    private def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.flatMap {
            case variable if variablesMap.contains(variable) =>
                val variableValue = variablesMap(variable)
                variableValue.toSeq.map(_.toString)

            case other =>
                Seq(other)
        }
    }

    private def solveExpression(expression: Seq[String]): String = {
        val substitutedExpression = substituteVariables(expression)
        val reversePolishExpression = expressionToRPN(substitutedExpression)
        val result = RPNToResult(reversePolishExpression)
        result.toString
    }

    override def readEval(command: String): String = {
        val tokens = command.split(" ")
        val isVariableAssignment = command.contains("=")
        val isSimplification = command.startsWith("@")

        if (isSimplification) {
            val expression = tokens.drop(1)
            val reversePolishExpression = expressionToRPN(expression).mkString(" ")
            val treeExpression = reversePolishToTreeExpression(reversePolishExpression)
            val simplifiedExpression = simplify(treeExpression).abstractToString
            return simplifiedExpression
        }

        val expression = if (isVariableAssignment) tokens.drop(2) else tokens
        val result = solveExpression(expression)

        if (isVariableAssignment) {
            val variableName = tokens.head
            variablesMap(variableName) = MultiSet(expression)
            s"$variableName = $result"

        } else {
            result
        }
    }
}