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

    override def pushBaseValueToBaseStack(outputStack: mutable.Stack[Base], element: String): Unit = {
        outputStack.push(element.toInt)
    }

    // Polish to Result/Expression Tree Code: https://gitlab.com/vu-oofp/lecture-code/-/blob/master/OOReversePolish.scala
    private def RPNToResult(expression: Seq[String]): Base = {
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

    private def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.map {
            case variable if variablesMap.contains(variable) =>
                variablesMap(variable).toString
            case other =>
                other
        }
    }

    private def solveExpression(expression: Seq[String]): String = {
        val substitutedExpression = substituteVariables(expression)
        val reversePolishExpression = expressionToRPN(substitutedExpression)
        val result = RPNToResult(reversePolishExpression)
        result.toString
    }

    override def readEval(command: String): String = {

        val tokens = command.split(" ").toList
        val isVariableAssignment = tokens.contains("=") // We check for assignments
        val isSimplification = command.contains("@") // We check for simplifications

        if (isSimplification) { // Check for simplification
            val expression = tokens.drop(1)
            val reversePolishExpression = expressionToRPN(expression).mkString(" ")
            val treeExpression = reversePolishToTreeExpression(reversePolishExpression) // We use the given code from the course
            val simplifiedExpression = simplify(treeExpression).abstractToString
            return simplifiedExpression
        }

        val expression = if (isVariableAssignment) tokens.drop(2) else tokens
        val result = solveExpression(expression)

        if (isVariableAssignment) {
            val variableName = tokens.head
            variablesMap(variableName) = result.toInt
            s"$variableName = $result"
        } else { // Else we work like normal evaluations
            result
        }
    }
}
