package repls
import scala.collection.mutable

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "int-repl"
    private val variables = mutable.Map[String, Int]() // Dictionary to store variables and their values

    def isOperator(char: String): Boolean = Set("+", "-", "*", "/").contains(char)

    def isInteger(char: String): Boolean = char.matches("-?\\d+")

    def precedence(operator: String): Int = operator match {
        case "+" | "-" => 1
        case "*" | "/" => 2
        case _ => 0 // Default precedence
    }

    def applyOperation(operand1: Int, operator: String, operand2: Int): Int = {
        operator match {
            case "+" => operand1 + operand2
            case "-" => operand1 - operand2
            case "*" => operand1 * operand2
            case "/" => operand1 / operand2
            case _ => throw new IllegalArgumentException(s"Invalid operator: $operator")
        }
    }

    // Shunting Yard Algorithm Pseudocode: https://aquarchitect.github.io/swift-algorithm-club/Shunting%20Yard/
    private def expressionToRPN (expression: Seq[String]): Seq[String] = {
        val outputStack = mutable.Stack[String]()
        val operatorStack = mutable.Stack[String]()

        expression.foreach {
            case token if isInteger(token) =>
                outputStack.push(token)

            case token if isOperator(token) =>
                while (operatorStack.nonEmpty && precedence(operatorStack.top) >= precedence(token)) {
                    val operator = operatorStack.pop()
                    outputStack.push(operator)
                }
                operatorStack.push(token)

            case "(" =>
                operatorStack.push("(")

            case ")" =>
                while (operatorStack.nonEmpty && operatorStack.top != "(") {
                    val operator = operatorStack.pop()
                    outputStack.push(operator)
                }
                if (operatorStack.isEmpty || operatorStack.top != "(") {
                    throw new IllegalArgumentException("Mismatched parentheses")
                }
                operatorStack.pop()
        }

        while (operatorStack.nonEmpty) {
            if (operatorStack.top == "(" || operatorStack.top == ")") {
                throw new IllegalArgumentException("Mismatched parentheses")
            }
            val operator = operatorStack.pop()
            outputStack.push(operator)
        }

        outputStack.reverse.toSeq
    }

    // Polish to Expression Tree Code: https://gitlab.com/vu-oofp/lecture-code/-/blob/master/OOReversePolish.scala
    private def RPNToTree(expression: Seq[String]): String = {
        val outputStack = mutable.Stack[String]()
        expression.foreach {
            case token if isOperator(token) =>
                val firstOperand = outputStack.pop()
                val secondOperand = outputStack.pop()
                val res = applyOperation(secondOperand.toInt, token, firstOperand.toInt)
                outputStack.push(res.toString)

            case token if isInteger(token) =>
                outputStack.push(token)

            case _ =>
                throw new Error("Unknown expression element")
        }
        outputStack.top
    }

    private def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.map {
            case variable if variables.contains(variable) =>
                variables(variable).toString
            case other =>
                other
        }
    }

    override def readEval(command: String): String = {
        val expression = SplitExpressionString.splitExpressionString(command)

        if (expression.length >= 3 && expression(1) == "=") {
            // Extract variable name and expression
            val variableName = expression.head
            val variableExpression = expression.drop(2)

            val substitutedExpression = substituteVariables(variableExpression)
            val reversePolishExpression = expressionToRPN(substitutedExpression)
            val result = RPNToTree(reversePolishExpression)
            variables(variableName) = result.toInt
            s"$variableName = $result"

        } else {

            val substitutedExpression = substituteVariables(expression)
            val reversePolishExpression = expressionToRPN(substitutedExpression)
            val result = RPNToTree(reversePolishExpression)
            if (variables.contains(result)) s"$result = ${variables(result)}" else result

        }
    }
}
