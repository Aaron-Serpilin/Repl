package repls
import scala.collection.mutable

abstract class REPLBase extends REPL {

    type Base
    // Dictionary to store variablesMap for Integers and MultiSets
    val variablesMap: mutable.Map[String, Base] = mutable.Map[String, Base]()
    // Operation handler for Base type
    val operationHandler: Map[String, (Base, Base) => Base]
    // Handler for empty values (0 or {})
    val emptyValue: Base

    // Repeated Code for the IntRepl and MultiSetRepl
    def isOperator(char: String): Boolean = Set("+", "-", "*", "/").contains(char)
    def isInteger(char: String): Boolean = char.matches("-?\\d+")
    def isVariable(char: String): Boolean = char.matches("[a-zA-Z0-9]+")

    private def precedence(operator: String): Int = operator match {
        case "+" | "-" => 1
        case "*" | "/" => 2
        case _ => 0 // Default precedence
    }

    // Shunting Yard Algorithm Pseudocode: https://aquarchitect.github.io/swift-algorithm-club/Shunting%20Yard/
    def expressionToRPN(expression: Seq[String]): Seq[String] = {

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

    def applyOperation (firstOperand: Base, operator: String, secondOperand: Base): Base = {
        operator match {
            case "+" => operationHandler("+")(firstOperand, secondOperand)
            case "-" => operationHandler("-")(firstOperand, secondOperand)
            case "*" => operationHandler("*")(firstOperand, secondOperand)
        }
    }


    abstract class Expression {
        def abstractToString: String
    }

    case class Const(number: Base) extends Expression {
        override def abstractToString: String = number.toString
    }

    private case class Var(string: String) extends Expression {
        override def abstractToString: String = string
    }

    private case class Operator(firstOperand: Expression, operatorName: String, secondOperand: Expression) extends Expression {

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

    def simplify(expression: Expression): Expression = {

            val simplifiedExpression = expression match {

                // Cases for Multi sets basic simplification
                case Operator(a, "*", _) if a.abstractToString == "{}" => a // {} * e → {}
                case Operator(_, "*", a) if a.abstractToString == "{}" => a // e * {} → {}
                case Operator(a1, "+", a2) if a2.abstractToString == "{}" => a1 // e + {} → e
                case Operator(a1, "+", a2) if a1.abstractToString == "{}" => a2 // {} + e → e
                case Operator(a1, "-", a2) if a1 == a2 && a1.abstractToString.contains("{") && a2.abstractToString.contains("{") => Var("{}") // e - e → {}
                case Operator(a1, "*", a2) if a1 == a2 && !isInteger(a1.abstractToString) && !isInteger(a2.abstractToString)=> a1 // e * e → e (We add the isInteger constraint to not alter normal integer multiplication)

                // Cases for Multi sets basic arithmetic
                //case Operator(a1, "+", a2) => Var(MultiSet(a1) + MultiSet(a2))
//                        case Operator(firstOperand, "*", secondOperand) if firstOperand == secondOperand => simplify(firstOperand)
//                        case Operator(a1, operatorName, a2) => Operator(simplify(a1), operatorName, simplify(a2))

                // Basic Arithmetic operations
                case Operator(Const(a), "+", Const(b)) => Const(operationHandler("+")(a, b))
                case Operator(Const(a), "-", Const(b)) => Const(operationHandler("-")(a, b))
                case Operator(Const(a), "*", Const(b)) => Const(operationHandler("*")(a, b))

                // Cases for Distributivity Simplification
                case Operator(Operator(a1, "*", b), "+", Operator(a2, "*", c))
                    if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( a * b ) + ( a * c ) → a * ( b + c )
                case Operator(Operator(b, "*", a1), "+", Operator(a2, "*", c))
                    if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( b * a ) + ( a * c ) → a * ( b + c )
                case Operator(Operator(a1, "*", b), "+", Operator(c, "*", a2))
                    if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( a * b ) + ( c * a ) → a * ( b + c )
                case Operator(Operator(b, "*", a1), "+", Operator(c, "*", a2))
                    if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( b * a ) + ( c * a ) → a * ( b + c )

                // Cases for addition and multiplication identity
                case Operator(expr, "+", Const(0)) => simplify(expr)
                case Operator(Const(0), "+", expr) => simplify(expr)
                case Operator(expr, "*", Const(1)) => simplify(expr)
                case Operator(Const(1), "*", expr) => simplify(expr)
                case Operator(_, "*", Const(0)) => Const(emptyValue)
                case Operator(Const(0), "*", _) => Const(emptyValue)

                // Case for subtracting identical expressions
                case Operator(lhs, "-", rhs) if lhs == rhs => Const(emptyValue)

                // Case for variable in variablesMap
                case Var(variable) if variablesMap.contains(variable) => Const(variablesMap(variable))
                case Var(variable) if variablesMap.contains(variable) =>
                    val multiSetValue = variablesMap(variable)
                    val varString = multiSetValue.toString.stripPrefix("{").stripSuffix("}") // To achieve the desired structure by the tests
                    Var(varString)

                // General case for binary operators
                case Operator(lhs, op, rhs) =>
                    val simplifiedLhs = simplify(lhs)
                    val simplifiedRhs = simplify(rhs)

                    (simplifiedLhs, simplifiedRhs) match {
                        case (Const(a), Const(b)) if Set("+", "-", "*", "/").contains(op) =>
                            Const(operationHandler(op)(a, b)) // Simplify the operation with constants of a sequence of operations
                        case _ =>
                            Operator(simplifiedLhs, op, simplifiedRhs) // Return the normal operation between two operands
                    }

                // Default case, non-simplifiable
                case _ => expression
            }

            // We recursively reduce the expression if it is not completely reduced already
            if (simplifiedExpression != expression) {
                simplify(simplifiedExpression)
            } else {
                simplifiedExpression
            }
        }

    private def isChar(string: String): Boolean = {
        string match {
            case "(" | ")" | "," | "+" | "-" | "*" | "/" | "=" | "@" => false
            case _ => true
        }
    }

    def pushBaseTypeValue (outputStack: mutable.Stack[Expression], element: String): Unit

    def reversePolishToTreeExpression(expression: String): Expression = {
        val outputStack: mutable.Stack[Expression] = new mutable.Stack()
        for (element <- expression.split(" ")) {
            if (!isChar(element)) {
                val secondOperand = outputStack.pop
                val firstOperand = outputStack.pop
                val result = Operator(firstOperand, element, secondOperand)
                outputStack.push(result)
            } else if (isChar(element)) {
                if (element.forall(_.isDigit)) {
                    pushBaseTypeValue(outputStack, element)
//                            outputStack.push(Const(element.toInt))
                } else {
                    outputStack.push(Var(element))
                }

            } else throw new Error("Unknown expression element " + element)
        }
        outputStack.top
    }

}
