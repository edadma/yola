package xyz.hyperreal.yola

import scala.collection.mutable

class Interpreter {

  val vars = new mutable.HashMap[String, Any]

  vars("println") = (args: List[Any]) => println(args)

  def apply(ast: AST): Unit = ast match {
    case SourceAST(stmts)           => stmts foreach apply
    case DeclarationBlockAST(decls) => decls map apply
    case VarAST(pos, name, None) =>
      if (vars contains name)
        problem(pos, s"duplicate declaration: '$name'")

      vars(name) = Holder(0)
    case VarAST(pos, name, Some((pose, exp))) =>
      if (vars contains name)
        problem(pos, s"duplicate declaration: '$name'")

      vars(name) = Holder(eval(exp))
    case DefAST(pos, name, func) =>
      if (vars contains name)
        problem(pos, s"duplicate declaration: '$name'")

      vars(name) = func
    case exp: ExpressionAST => eval(exp)
  }

  def eval(exp: ExpressionAST): Any = exp match {
    case BinaryExpressionAST(lpos, left, op, rpos, right) =>
      val l = eval(left)
      val r = eval(right)

      op match {
        case "+" => l.asInstanceOf[Int] + r.asInstanceOf[Int]
      }
    case VariableExpressionAST(pos, name) =>
      vars get name match {
        case None            => problem(pos, s"unknown variable '$name'")
        case Some(Holder(v)) => v
        case Some(v)         => v
      }
    case LiteralExpressionAST(v) => v
    case ListExpressionAST(l)    => l map eval
    case ApplyExpressionAST(epos, f, apos, args, tailrecursive) =>
      val args1 = args map { case (_, e) => eval(e) }

      eval(f) match {
        case func: (List[Any] => Any)                                   => func(args1.asInstanceOf[List[Any]])
        case FunctionExpressionAST(pos, name, parms, arb, parts, where) =>
      }
  }

  case class Holder(var v: Any)
}
