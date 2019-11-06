package xyz.hyperreal.yola

import scala.collection.mutable

class Interpreter {

  val varMap = new mutable.HashMap[String, Any]

  varMap("println") = (args: List[Any]) => println(args)

  def apply(ast: AST): Unit = ast match {
    case SourceAST(stmts)                     => stmts foreach apply
    case DeclarationBlockAST(decls)           => decls map apply
    case VarAST(pos, name, None)              => varMap(name) = Holder(0)
    case VarAST(pos, name, Some((pose, exp))) => varMap(name) = Holder(eval(exp))
    case ApplyExpressionAST(epos, f, apos, args, tailrecursive) =>
      val args1 = args map { case (_, e) => eval(e) }

      eval(f) match {
        case func: (Any => Any) => func(args1.asInstanceOf[List[Any]])
      }
  }

  def eval(exp: ExpressionAST): Any = exp match {
    case BinaryExpressionAST(lpos, left, op, rpos, right) =>
      val l = eval(left)
      val r = eval(right)

      op match {
        case "+" => l.asInstanceOf[Int] + r.asInstanceOf[Int]
      }
    case VariableExpressionAST(pos, name) =>
      varMap get name match {
        case None            => problem(pos, s"unknown variable '$name'")
        case Some(Holder(v)) => v
        case Some(v)         => v
      }
    case LiteralExpressionAST(v) => v
    case ListExpressionAST(l)    => l map eval
  }

  case class Holder(var v: Any)
}

/*

SourceAST(
  List(
    DeclarationBlockAST(
      List(
        ValAST(
          VariableStructureAST(a),
          LiteralExpressionAST(123)
        )
      )
    ),
    ApplyExpressionAST(
      VariableExpressionAST(println),
      List((4.10,BinaryExpressionAST(4.10,VariableExpressionAST(4.10,a,a),'+,4.14,LiteralExpressionAST(1)))),false), DefAST(f,FunctionExpressionAST(f,6.5,List(VariableStructureAST(6.7,x,x)),false,List(FunctionPartExpressionAST(None,BinaryExpressionAST(6.12,VariableExpressionAST(6.12,x,x),'+,6.16,LiteralExpressionAST(3)))),WhereClauseAST(List())))))

 */
