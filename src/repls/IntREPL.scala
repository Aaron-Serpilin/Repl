package repls
import scala.collection.mutable

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "Base-repl"
    private val variablesMap = mutable.Map[String, Base]() // Dictionary to store variablesMap and their values

    private def isOperator(char: String): Boolean = Set("+", "-", "*", "/").contains(char)
    private def isInteger(char: String): Boolean = char.matches("-?\\d+")
    private def isVariable(char: String): Boolean = char.matches("[a-zA-Z0-9]+")

    private def precedence(operator: String): Base = operator match {
        case "+" | "-" => 1
        case "*" | "/" => 2
        case _ => 0 // Default precedence
    }

    private def applyOperation(firstOperator: Base, operator: String, secondOperator: Base): Base = operator match {
        case "+" => firstOperator + secondOperator
        case "-" => firstOperator - secondOperator
        case "*" => firstOperator * secondOperator
        case "/" => firstOperator / secondOperator
    }

    // Shunting Yard Algorithm Pseudocode: https://aquarchitect.github.io/swift-algorithm-club/Shunting%20Yard/
    private def expressionToRPN(expression: Seq[String]): Seq[String] = {

        val outputStack = mutable.Stack[String]()
        val operatorStack = mutable.Stack[String]()

        expression.foreach {
            case token if isInteger(token) || isVariable(token) => outputStack.push(token)

            case token if isOperator(token) =>
                while (operatorStack.nonEmpty && precedence(operatorStack.top) >= precedence(token)) {
                    val operator = operatorStack.pop()
                    outputStack.push(operator)
                }
                operatorStack.push(token)

            case "(" => operatorStack.push("(")

            case ")" =>
                while (operatorStack.nonEmpty && operatorStack.top != "(") {
                    val operator = operatorStack.pop()
                    outputStack.push(operator)
                }
                operatorStack.pop()
        }

        while (operatorStack.nonEmpty) {
            val operator = operatorStack.pop()
            outputStack.push(operator)
        }

        outputStack.reverse.toSeq
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
            val treeExpression = repls.Expressions.ReversePolish.reversePolishToExpression(reversePolishExpression) // We use the given code from the course
            val simplifiedExpression = repls.Expressions.PatternMatch.simplify(treeExpression, variablesMap).abstractToString
            repls.Expressions
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
