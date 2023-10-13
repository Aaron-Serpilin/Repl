package repls
import scala.collection.mutable

object Expressions {
  abstract class Expression {
    def evaluate(variableMap: mutable.Map[String, Int]): Int
    def abstractToString: String
  }

  case class Const (number: Int) extends Expression {
    override def evaluate(variableMap: mutable.Map[String, Int]): Int = number
    override def abstractToString: String = number.toString
  }

  case class Var (string: String) extends Expression {
    override def evaluate(variableMap: mutable.Map[String, Int]): Int = variableMap(string)
    override def abstractToString: String = string
  }

  case class Negate (arg: Expression) extends Expression {
    override def evaluate(variableMap: mutable.Map[String, Int]): Int = -arg.evaluate(variableMap)
    override def abstractToString: String = s"-${arg.abstractToString}"
  }

  case class Operator (firstOperand: Expression, operatorName: String, secondOperand: Expression) extends Expression {
    override def evaluate(variableMap: mutable.Map[String, Int]): Int = {
      val l = firstOperand.evaluate(variableMap)
      val r = secondOperand.evaluate(variableMap)
      PatternMatch.operatorByName(l, operatorName, r)
    }
    override def abstractToString: String = s"${firstOperand.abstractToString} $operatorName ${secondOperand.abstractToString}"
  }

  object PatternMatch {

    private def isOperator (string: String) = string == "+" || string == "*" || string == "/"

    def operatorByName(l: Int, name: String, r: Int): Int = {
      name match {
        case "+" => l + r
        case "-" => l - r
        case "*" => l * r
        case "/" => l / r
      }
    }

    private def evaluate(variableMap: mutable.Map[String, Int], expression: Expression): Int =
      expression match {
        case Const(i) => i
        case Var(s) => variableMap(s)
        case Negate(arg) => -evaluate(variableMap, arg)
        case Operator(firstOperand, op, secondOperand) =>
          operatorByName(evaluate(variableMap, firstOperand), op, evaluate(variableMap, secondOperand))
      }

    def simplify(expression: Expression, variableMap: mutable.Map[String, Int]): Expression = expression match {
      // Cases for simplification of computations
      case Operator(Const(a), "+", Const(b)) => Const(a + b)
      case Operator(Const(a), "-", Const(b)) => Const(a - b)
      case Operator(Const(a), "*", Const(b)) => Const(a * b)
      case Operator(Const(a), "/", Const(b)) => Const(a / b)

      // Cases for Redundant Zero simplification // Not working... Implemented below as well
//      case Operator(Const(0), "+", expr) => simplify(expr, variableMap)
//      case Operator(expr, "+", Const(0)) => simplify(expr, variableMap)
//      case Operator(Const(0), "-", expr) => simplify(expr, variableMap)
//      case Operator(expr, "-", Const(0)) => simplify(expr, variableMap)

      // Cases for Negate simplification
      case Negate(Negate(expr)) => simplify(expr, variableMap)
      case Negate(expr) => Negate(simplify(expr, variableMap))

      // Cases for addition and multiplication identity
      case Operator(expr, "+", Const(0)) => simplify(expr, variableMap)
      case Operator(Const(0), "+", expr) => simplify(expr, variableMap)
      case Operator(expr, "*", Const(1)) => simplify(expr, variableMap)
      case Operator(Const(1), "*", expr) => simplify(expr, variableMap)
      case Operator(_, "*", Const(0)) => Const(0)
      case Operator(Const(0), "*", _) => Const(0)

      // Case for subtracting identical expressions
      case Operator(lhs, "-", rhs) if lhs == rhs => Const(0)

      // Case for variable in variablesMap
      case Var(variable) if variableMap.contains(variable) => Const(variableMap(variable))

      // General case for binary operators
      case Operator(lhs, op, rhs) =>
        val simplifiedLhs = simplify(lhs, variableMap)
        val simplifiedRhs = simplify(rhs, variableMap)
        if (isOperator(op) && simplifiedLhs.isInstanceOf[Const] && simplifiedRhs.isInstanceOf[Const]) { // We checks for large sequences of operations
          Const(operatorByName(simplifiedLhs.evaluate(variableMap), op, simplifiedRhs.evaluate(variableMap)))
        } else {
          Operator(simplifiedLhs, op, simplifiedRhs) // We return the normal operation between two operands
        }

      // Default case, non-simplifiable
      case _ => expression
    }

  }

  object ReversePolish {

    def isChar(string: String): Boolean = {
      string match {
        case "(" | ")" | "," | "+" | "-" | "*" | "/" | "=" | "@" => false
        case _ => true
      }
    }

    def reversePolishToExpression(expression: String): Expression = {
      val outputStack: mutable.Stack[Expression] = new mutable.Stack()
      for (element <- expression.split(" ")) {
        if (!isChar(element)) {
          val secondOperand = outputStack.pop
          val firstOperand = outputStack.pop
          val res = Operator(firstOperand, element, secondOperand)
          outputStack.push(res)
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


