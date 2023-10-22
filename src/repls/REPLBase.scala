package repls
import scala.collection.mutable

abstract class REPLBase extends REPL {

    type Base
    val variablesMap: mutable.Map[String, Base] = mutable.Map[String, Base]() // Dictionary to store variablesMap for Integers and MultiSets
    val operationHandler: Map[String, (Base, Base) => Base]  // Operation handler for Base type
    val emptyValue: Base // Handler for empty values (0 or {})

    // Repeated Code for the IntRepl and MultiSetRepl
    def isOperator(char: String): Boolean = Set("+", "-", "*", "/").contains(char)
    def isInteger(char: String): Boolean = char.matches("-?\\d+")
    def isVariable(char: String): Boolean = char.matches("[a-zA-Z0-9]+")
    def pushBaseValueToExpressionStack(outputStack: mutable.Stack[Expression], element: String): Unit // Function to push base type values to the expression stack
    def RPNToResult(expression: Seq[String]): Base // Polish to Result/Expression Tree Code: https://gitlab.com/vu-oofp/lecture-code/-/blob/master/OOReversePolish.scala
    def substituteVariables(expression: Seq[String]): Seq[String]
    def assignBaseVariable (expression: Array[String], result: String, variableName: String): Unit
    def simplify (expression: Expression): Expression

    private def precedence(operator: String): Int = operator match {
        case "+" | "-" => 1
        case "*" | "/" => 2
        case _ => 0 // Default precedence
    }

    // Shunting Yard Algorithm Pseudocode: https://aquarchitect.github.io/swift-algorithm-club/Shunting%20Yard/
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

    def applyOperation(firstOperand: Base, operator: String, secondOperand: Base): Base = {
        operator match {
            case "+" => operationHandler("+")(firstOperand, secondOperand)
            case "-" => operationHandler("-")(firstOperand, secondOperand)
            case "*" => operationHandler("*")(firstOperand, secondOperand)
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
            assignBaseVariable(expression, result, variableName) // The IntRepl needs the result while the MultiSetRepl the expression. So in the overrides they use the right value
            s"$variableName = $result"

        } else {
            result
        }
    }

    // Code for all the classes
    abstract class Expression {def abstractToString: String}

    case class Const(number: Base) extends Expression {
        override def abstractToString: String = number.toString
    }

    case class Var(string: String) extends Expression {
        override def abstractToString: String = string
    }

    case class Operator (firstOperand: Expression, operatorName: String, secondOperand: Expression) extends Expression {
        override def abstractToString: String = { // Correctly prints out the simplifications. Have to check for parenthesis placement for the distributivity rules
            val shouldParenthesisBePlaced = ((operatorName == "*" || operatorName == "/")
              && (secondOperand.isInstanceOf[Operator] && (secondOperand.asInstanceOf[Operator].operatorName == "+" || secondOperand.asInstanceOf[Operator].operatorName == "-")))

            val addLeftParenthesis = firstOperand.isInstanceOf[Operator] &&
              (firstOperand.asInstanceOf[Operator].operatorName == "+" || firstOperand.asInstanceOf[Operator].operatorName == "-") &&
              (operatorName == "*" || operatorName == "/")

            val leftOperand = if (addLeftParenthesis && !firstOperand.isInstanceOf[Var]) s"( ${firstOperand.abstractToString} )" else firstOperand.abstractToString
            val rightOperand = if (shouldParenthesisBePlaced) s"( ${secondOperand.abstractToString} )" else secondOperand.abstractToString

            s"$leftOperand $operatorName $rightOperand"
        }
    }

    private def isChar (string: String): Boolean = {
        string match {
            case "(" | ")" | "," | "+" | "-" | "*" | "/" | "=" | "@" => false
            case _ => true
        }
    }

    private def reversePolishToTreeExpression (expression: String): Expression = {
        val outputStack: mutable.Stack[Expression] = new mutable.Stack()
        for (element <- expression.split(" ")) {
            if (!isChar(element)) {
                val secondOperand = outputStack.pop
                val firstOperand = outputStack.pop
                val result = Operator(firstOperand, element, secondOperand)
                outputStack.push(result)
            } else if (isChar(element)) {
                if (element.forall(_.isDigit)) {
                    pushBaseValueToExpressionStack(outputStack, element)
                } else {
                    outputStack.push(Var(element))
                }

            } else throw new Error("Unknown expression element " + element)
        }
        outputStack.top
    }
}
