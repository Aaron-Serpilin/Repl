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

    def operatorByName(l: Int, name: String, r: Int): Int = {
      name match {
        case "+" => l + r
        case "-" => l - r
        case "*" => l * r
        case "/" => l / r
      }
    }

    def evaluate(variableMap: mutable.Map[String, Int], expression: Expression): Int =
      expression match {
        case Const(i) => i
        case Var(s) => variableMap(s)
        case Negate(arg) => -evaluate(variableMap, arg)
        case Operator(firstOperand, op, secondOperand) =>
          operatorByName(evaluate(variableMap, firstOperand), op, evaluate(variableMap, secondOperand))
      }

    def simplify(expression: Expression): Expression =
      expression match {
        // Cases for simplification of computations
        case Operator(Const(firstOperand), "+", Const(secondOperand)) => Const(firstOperand + secondOperand)
        case Operator(Const(firstOperand), "-", Const(secondOperand)) => Const(firstOperand - secondOperand)
        case Operator(Const(firstOperand), "*", Const(secondOperand)) => Const(firstOperand * secondOperand)
        case Operator(Const(firstOperand), "/", Const(secondOperand)) => Const(firstOperand / secondOperand)

        // Cases for basic arithmetic simplification
        case Negate(Negate(e)) => simplify(e)
        case Negate(e) => Negate(simplify(e))
        case Operator(e, "+", Const(0)) => simplify(e) //    e + 0 → e
        case Operator(Const(0), "+", e) => simplify(e) //    0 + e → e
        case Operator(e, "*", Const(1)) => simplify(e) //    e * 1 → e
        case Operator(Const(1), "*", e) => simplify(e) //    1 * e → e
        case Operator(e, "*", Const(0)) => Const(0) //    e * 0 → 0
        case Operator(Const(0), "*", e) => Const(0) //    0 * e → 0
        case Operator(lhs, "-", rhs) if lhs == rhs => Const(0) // e - e -> 0
        case Operator(l, op, r) => Operator(simplify(l), op, simplify(r))
        case _ => expression
      }

  }

  object ReversePolish {

    private def isOperator (string: String) = string == "+" || string == "*" || string == "/"

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


