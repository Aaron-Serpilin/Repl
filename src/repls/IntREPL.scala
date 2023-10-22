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

    override def assignBaseVariable (expression: Array[String], result: String, variableName: String): Unit = {
        variablesMap(variableName) = result.toInt
    }

    override def RPNToResult(expression: Seq[String]): Base = {
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

    override def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.map {
            case variable if variablesMap.contains(variable) =>
                variablesMap(variable).toString
            case other =>
                other
        }
    }

    override def simplify(expression: Expression): Expression = {

        val simplifiedExpression = expression match {

            // Basic Arithmetic operations
            case Operator(Const(a), "+", Const(b)) => simplify(Const(operationHandler("+")(a, b)))
            case Operator(Const(a), "-", Const(b)) => simplify(Const(operationHandler("-")(a, b)))
            case Operator(Const(a), "*", Const(b)) => simplify(Const(operationHandler("*")(a, b)))

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
}
