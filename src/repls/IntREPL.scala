package repls
import scala.collection.mutable
import scala.collection.mutable.Stack

class IntREPL extends REPLBase {

    type Base = Int
    override val replName: String = "int-repl"

    private def normalToReversePolish(expression: Seq[String]): Seq[String] = {
        val outputStack = mutable.Stack[String]()
        val operatorStack = mutable.Stack[String]()

        // Shunting Yard Algorithm Pseudocode: https://aquarchitect.github.io/swift-algorithm-club/Shunting%20Yard/
        for (token <- expression) {
            if (isInteger(token)) {
                outputStack.push(token)
            } else if (isOperator(token)) {
                while (operatorStack.nonEmpty && (precedence(operatorStack.top) >= precedence(token))) {
                    val operator = operatorStack.pop()
                    outputStack.push(operator)
                }
                operatorStack.push(token)
            } else if (token == "(") {
                operatorStack.push(token)
            } else if (token == ")") {
                while (operatorStack.nonEmpty && operatorStack.top != "(") {
                    val operator = operatorStack.pop()
                    outputStack.push(operator)
                }
                operatorStack.pop()
            }
        }

        while (operatorStack.nonEmpty) {
            if (operatorStack.top == "(" || operatorStack.top == ")") {
                throw new IllegalArgumentException("Mismatched parentheses")
            }
            val operator = operatorStack.pop()
            outputStack.push(operator)
        }

        val sequencedOutput: Seq[String] = outputStack.reverse.toSeq
        sequencedOutput

    }

    private def reversePolishToExpressionTree(expression: Seq[String]): String = {
        val outputStack = mutable.Stack[String]()
        for (token <- expression) {
            if (isOperator(token)) {
                val firstOperand = outputStack.pop()
                val secondOperand = outputStack.pop()
                val res = applyOperation(secondOperand.toInt, token, firstOperand.toInt)
                outputStack.push(res.toString)
            } else if (isInteger(token)) {
                outputStack.push(token)
            } else {
                throw new Error("Unknown expression element " + token)
            }
        }
        outputStack.top
    }


    override def readEval(command: String): String = {
        val expression = SplitExpressionString.splitExpressionString(command)
        val reversePolishExpression = normalToReversePolish(expression)
        val standardExpression = reversePolishToExpressionTree(reversePolishExpression)
        //val result = evaluateExpression(standardExpression)
        //println(s"The result is ${reversedExpression}")
        //println(s"The standard expression is ${standardExpression}")
        //result
        standardExpression
    }

    def evaluateExpression(expression: Seq[String]): String = {
        val operandStack = mutable.Stack[Int]() // Stack to store operands
        val operatorStack = mutable.Stack[String]() // Stack to store operators

        ""
    }

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
}
