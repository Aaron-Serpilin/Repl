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

    private def applyOperation(firstOperator: Int, operator: String, secondOperator: Int): Int = operator match {
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
            case token if isInteger(token) => outputStack.push(token)

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

    // Function to simplify an RPN expression
    private def simplifyRPN(expression: Seq[String]): Seq[String] = {
        val simplifiedStack = mutable.Stack[String]()

        simplifiedStack.toSeq
    }


    // Polish to Expression Tree Code: https://gitlab.com/vu-oofp/lecture-code/-/blob/master/OOReversePolish.scala
    private def RPNToTree(expression: Seq[String]): Seq[String] = {
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
            case variable if variables.contains(variable) =>
                variables(variable).toString
            case other =>
                other
        }
    }

    private def solveExpression (expression: Seq[String], isSimplification: Boolean): String = {
        val substitutedExpression = substituteVariables(expression)
        val reversePolishExpression = expressionToRPN(substitutedExpression)
        val treeExpression = RPNToTree(reversePolishExpression)
        if (isSimplification) simplifyRPN(treeExpression).head else treeExpression.head
    }

    override def readEval(command: String): String = {
        val tokens = command.split(" ").toList
        val isSimplification = tokens.contains("@")
        val expression = if (tokens.contains("=")) {
            tokens.drop(2)
        } else {
            tokens.drop(if (isSimplification) 1 else 0)
        }
        val result = solveExpression(expression, isSimplification)

        if (tokens.contains("=")) { // We check for assignments
            val variableName = tokens.head
            variables(variableName) = result.toInt
            s"$variableName = $result"
        } else { // Else we work like normal evaluations
            result
        }
    }

}
