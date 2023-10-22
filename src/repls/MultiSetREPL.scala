package repls
import scala.collection.mutable
import repls.MultiSet.empty

class MultiSetREPL extends REPLBase {

    override type Base = MultiSet[String]
    override val replName: String = "multiset-repl"
    override val operationHandler: Map[String, (Base, Base) => Base] = Map(
        "+" -> ((a, b) => a + b),
        "-" -> ((a, b) => a - b),
        "*" -> ((a, b) => a * b)
    )
    override val emptyValue: MultiSet[String] = empty[String]

    override def pushBaseValueToExpressionStack(outputStack: mutable.Stack[Expression], element: String): Unit = {
        val elementAsSeq = Seq(element)
        outputStack.push(Const(MultiSet(elementAsSeq)))
    }

    def assignBaseVariable (expression: Array[String], result: String, variableName: String): Unit = {
        variablesMap(variableName) = MultiSet(expression)
    }

    override def RPNToResult(expression: Seq[String]): Base = {
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

    override def substituteVariables(expression: Seq[String]): Seq[String] = {
        expression.flatMap {
            case variable if variablesMap.contains(variable) =>
                val variableValue = variablesMap(variable)
                variableValue.toSeq.map(identity)

            case other =>
                Seq(other)
        }
    }

    override def simplify(expression: Expression): Expression = {

        val simplifiedExpression = expression match {

            // Basic Arithmetic operations
            case Operator(Const(a), "+", Const(b)) => simplify(Const(operationHandler("+")(a, b)))
            case Operator(Const(a), "-", Const(b)) => simplify(Const(operationHandler("-")(a, b)))
            case Operator(Const(a), "*", Const(b)) => simplify(Const(operationHandler("*")(a, b)))

            // Cases for Multi sets identities
            case Operator(a, "*", _) if a.abstractToString == "{}" => a // {} * e → {}
            case Operator(_, "*", a) if a.abstractToString == "{}" => a // e * {} → {}
            case Operator(a1, "+", a2) if a2.abstractToString == "{}" => a1 // e + {} → e
            case Operator(a1, "+", a2) if a1.abstractToString == "{}" => a2 // {} + e → e
            case Operator(a1, "-", a2) if a1 == a2 && a1.abstractToString.contains("{") && a2.abstractToString.contains("{") => Var("{}") // e - e → {}
            case Operator(a1, "*", a2) if a1 == a2 && !isInteger(a1.abstractToString) && !isInteger(a2.abstractToString) => a1 // e * e → e (We add the isInteger constraint to not alter normal integer multiplication)

            // Case for subtracting identical expressions
            case Operator(lhs, "-", rhs) if lhs == rhs => Const(emptyValue)

            // Case for variable in variablesMap
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
}