package repls

abstract class Expression {
  def evaluate(variableMap : Map[String,Int]) : Int
}

case class constantExpression (constant: Int) extends  Expression {
  override def evaluate(variableMap: Map[String, Int]): Int = constant
}
case class variableExpression (variableName: String) extends Expression {
  override def evaluate(variableMap: Map[String, Int]): Int = variableMap(variableName)
}

case class negationExpression (negatedArgument: Expression) extends Expression {
  override def evaluate(variableMap: Map[String, Int]): Int = -negatedArgument.evaluate(variableMap)
}

case class operatorExpression (firstOperand: Expression, operator: String, secondOperand: Expression ) extends  Expression {
  override def evaluate(variableMap: Map[String, Int]): Int = {
    val firstValue = firstOperand.evaluate(variableMap)
    val secondValue = secondOperand.evaluate(variableMap)
    PatternMatch.operatorByName(firstValue, operator, secondValue)
  }
}

object PatternMatch {

  def operatorByName(firstValue : Int, name : String, secondValue : Int) : Int = {
    name match {
      case "+" => firstValue + secondValue
      case "-" => firstValue - secondValue
      case "*" => firstValue * secondValue
      case "/" => firstValue / secondValue
    }
  }

  def evaluate(variableMap: Map[String, Int], expression: Expression) : Int =
    expression match {
      case constantExpression(i) => i
      case variableExpression(s) => variableMap(s)
      case negationExpression(arg) => -evaluate(variableMap, arg)
      case operatorExpression(lhs, op , rhs) =>
        operatorByName(evaluate(variableMap,lhs), op, evaluate(variableMap, rhs))
    }
  
  def simplify(expression: Expression) : Expression = {
    expression match {
      case negationExpression(negationExpression(e)) => simplify(e)
      case operatorExpression(e, "+" , constantExpression(0)) => simplify(e)
      case operatorExpression(e, "*" , constantExpression(1)) => simplify(e)
      case negationExpression(e) => negationExpression(simplify(e))
      case operatorExpression(l,op,r) => operatorExpression(simplify(l), op, simplify(r))
      case _ => expression
    }
  }
}
