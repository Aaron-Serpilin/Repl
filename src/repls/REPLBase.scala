package repls
import scala.collection.mutable

abstract class REPLBase extends REPL {

    type Base
    // Dictionary to store variablesMap for Integers and MultiSets
    val variablesMap: mutable.Map[String, Base] = mutable.Map[String, Base]()
    
    // Repeated Code for the IntRepl and MultiSetRepl
    def isOperator(char: String): Boolean = Set("+", "-", "*", "/").contains(char)
    def isInteger(char: String): Boolean = char.matches("-?\\d+")
    def isVariable(char: String): Boolean = char.matches("[a-zA-Z0-9]+")

    def precedence(operator: String): Int = operator match {
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

    object Expressions {
        abstract class Expression {
            def abstractToString: String
        }

        case class Const(number: Int) extends Expression {
            override def abstractToString: String = number.toString
        }

        case class Var(string: String) extends Expression {
            override def abstractToString: String = string
        }

        case class Negate(arg: Expression) extends Expression {
            override def abstractToString: String = s"-${arg.abstractToString}"
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

        object PatternMatch {

            private def isOperator(string: String) = string == "+" || string == "*" || string == "/"

            def operatorByName(l: Int, name: String, r: Int): Int = {
                name match {
                    case "+" => l + r
                    case "-" => l - r
                    case "*" => l * r
                    case "/" => l / r
                }
            }

            def simplify(expression: Expression): Expression = {
                
                    val simplifiedExpression = expression match {

                        // Cases for Multi sets
                        case Operator(Var(a1), "*", Var(a2)) if a1 == a2 && a1.contains("{") && a2.contains("{") => Var(a1) // e * e → e
                        case Operator(Var(a), "*", Var(_)) if a == "{}" => Var(a) // {} * e → {}
                        case Operator(Var(_), "*", Var(a)) if a == "{}" => Var(a) // e * {} → {}
                        case Operator(Var(a1), "+", Var(a2)) if a2 == "{}" => Var(a1) // e + {} → e
                        case Operator(Var(a1), "+", Var(a2)) if a1 == "{}" => Var(a2) // {} + e → e
                        case Operator(Var(a1), "-", Var(a2)) if a1 == a2 && a1.contains("{") && a2.contains("{") => Var("{}") // e - e → {}
                        case Operator(Var(a1), "*", Var(a2)) if a1 == a2 => Var(a1) // e * e → e

                        // Cases for Distributivity Simplification
                        case Operator(Operator(a1, "*", b), "+", Operator(a2, "*", c))
                            if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( a * b ) + ( a * c ) → a * ( b + c )
                        case Operator(Operator(b, "*", a1), "+", Operator(a2, "*", c))
                            if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( b * a ) + ( a * c ) → a * ( b + c )
                        case Operator(Operator(a1, "*", b), "+", Operator(c, "*", a2))
                            if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( a * b ) + ( c * a ) → a * ( b + c )
                        case Operator(Operator(b, "*", a1), "+", Operator(c, "*", a2))
                            if a1 == a2 => Operator(a1, "*", Operator(b, "+", c)) // ( b * a ) + ( c * a ) → a * ( b + c )

                        // Cases for simplification of computations
                        case Operator(Const(a), "+", Const(b)) => Const(a + b)
                        case Operator(Const(a), "-", Const(b)) => Const(a - b)
                        case Operator(Const(a), "*", Const(b)) => Const(a * b)
                        case Operator(Const(a), "/", Const(b)) => Const(a / b)

                        // Cases for Negate simplification
                        case Negate(Negate(expr)) => simplify(expr)
                        case Negate(expr) => Negate(simplify(expr))

                        // Cases for addition and multiplication identity
                        case Operator(expr, "+", Const(0)) => simplify(expr)
                        case Operator(Const(0), "+", expr) => simplify(expr)
                        case Operator(expr, "*", Const(1)) => simplify(expr)
                        case Operator(Const(1), "*", expr) => simplify(expr)
                        case Operator(_, "*", Const(0)) => Const(0)
                        case Operator(Const(0), "*", _) => Const(0)

                        // Case for subtracting identical expressions
                        case Operator(lhs, "-", rhs) if lhs == rhs => Const(0)

                        // Case for variable in variablesMap
                        case Var(variable) if variablesMap.contains(variable) => Const(variablesMap(variable).toString.toInt)
                        case Var(variable) if variablesMap.contains(variable) =>
                            val multiSetValue = variablesMap(variable)
                            val varString = multiSetValue.toString.stripPrefix("{").stripSuffix("}") // To achieve the desired structure by the tests
                            Var(varString)

                        // General case for binary operators
                        case Operator(lhs, op, rhs) =>
                            val simplifiedLhs = simplify(lhs)
                            val simplifiedRhs = simplify(rhs)

                            (simplifiedLhs, simplifiedRhs) match {
                                case (Const(a), Const(b)) if isOperator(op) =>
                                    Const(operatorByName(a, op, b)) // Simplify the operation with constants of a sequence of operations
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
            

        }

        object ReversePolish {

            def isChar(string: String): Boolean = {
                string match {
                    case "(" | ")" | "," | "+" | "-" | "*" | "/" | "=" | "@" => false
                    case _ => true
                }
            }

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
                            outputStack.push(Const(element.toInt))
                        } else {
                            outputStack.push(Var(element))
                        }

                    } else throw new Error("Unknown expression element " + element)
                }
                outputStack.top
            }


        }
    }
}
