package repls
import scala.collection.mutable

class MultiSetREPL extends REPLBase {

    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"

    private val variablesMap = mutable.Map[String, Base]()

    private def isOperator(char: String): Boolean = Set("+", "-", "*", "/").contains(char)
    private def isVariable(char: String): Boolean = char.matches("[a-zA-Z0-9]+")
    private def precedence(operator: String): Int = operator match {
        case "+" | "-" => 1
        case "*" | "/" => 2
        case _ => 0 // Default precedence
    }

    private def applyOperation(firstOperator: Base, operator: String, secondOperator: Base): Base = operator match {
        case "+" => firstOperator + secondOperator
        case "-" => firstOperator - secondOperator
        case "*" => firstOperator * secondOperator
    }

    private def expressionToRPN(expression: Seq[String]): Seq[String] = {

        val outputStack = mutable.Stack[String]()
        val operatorStack = mutable.Stack[String]()

        expression.foreach {
            case token if isVariable(token) => outputStack.push(token)

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

            case token if token.length > 1 => outputStack.push(token) // Accounts for multi sets
        }

        while (operatorStack.nonEmpty) {
            val operator = operatorStack.pop()
            outputStack.push(operator)
        }

        outputStack.reverse.toSeq
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
        println(result.toString)
        result.toString
    }

    override def readEval(command: String): String = {
        println(command)
        val tokens = command.split(" ")
        val isVariableAssignment = command.contains("=")
        val isSimplification = command.contains("@")

        val expression = if (isVariableAssignment) tokens.drop(2) else tokens
        val result = solveExpression(expression)

        if (isVariableAssignment) {
            val variableName = tokens.head
            variablesMap(variableName) = MultiSet(expression)
            s"$variableName = $result"

//        } else if (isSimplification) {
//            val expression = tokens.drop(1)
//            val reversePolishExpression = expressionToRPN(expression).mkString(" ")
//            val treeExpression = repls.Expressions.ReversePolish.reversePolishToExpression(reversePolishExpression) // We use the given code from the course
//            val simplifiedExpression = repls.Expressions.PatternMatch.simplify(treeExpression, variablesMap).abstractToString
//            repls.Expressions
//            simplifiedExpression
        } else {
            result
        }
    }
}






