package repls
import scala.collection.mutable

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "Base-repl"

    private def applyOperation(firstOperator: Base, operator: String, secondOperator: Base): Base = operator match {
        case "+" => firstOperator + secondOperator
        case "-" => firstOperator - secondOperator
        case "*" => firstOperator * secondOperator
    }

    // Polish to Result/Expression Tree Code: https://gitlab.com/vu-oofp/lecture-code/-/blob/master/OOReversePolish.scala
    private def RPNToResult(expression: Seq[String]): Seq[String] = {
        val outputStack = mutable.Stack[String]()
        expression.foreach {
            case token if isOperator(token) =>
                val firstOperand = outputStack.pop()
                val secondOperand = outputStack.pop()
                val result = applyOperation(secondOperand.toInt, token, firstOperand.toInt)
                outputStack.push(result.toString)

            case token if isInteger(token) =>
                outputStack.push(token)

        }
        outputStack.toSeq
    }

    private def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.map {
            case variable if variablesMap.contains(variable) =>
                variablesMap(variable).toString
            case other =>
                other
        }
    }

    private def solveExpression (expression: Seq[String]): String = {
        val substitutedExpression = substituteVariables(expression)
        val reversePolishExpression = expressionToRPN(substitutedExpression)
        val result = RPNToResult(reversePolishExpression)
        result.head
    }

    override def readEval(command: String): String = {

        val tokens = command.split(" ").toList
        val isVariableAssignment = tokens.contains("=") // We check for assignments
        val isSimplification = command.contains("@") // We check for simplifications

        if (isSimplification) { // Check for simplification
            val expression = tokens.drop(1)
            val reversePolishExpression = expressionToRPN(expression).mkString(" ")
            val treeExpression = Expressions.ReversePolish.reversePolishToTreeExpression(reversePolishExpression) // We use the given code from the course
            val simplifiedExpression = Expressions.PatternMatch.simplify(treeExpression).abstractToString
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
