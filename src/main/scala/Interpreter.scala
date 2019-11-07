package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Interpreter {

  def apply(ast: AST)(implicit scope: Scope): Unit = ast match {
    case SourceAST(stmts)           => stmts foreach apply
    case DeclarationBlockAST(decls) => decls map apply
    case VarAST(pos, name, None) =>
      if (scope.vars contains name)
        problem(pos, s"duplicate declaration: '$name'")

      scope.vars(name) = Holder(0)
    case VarAST(pos, name, Some((pose, exp))) =>
      duplicate(pos, name)
      scope.vars(name) = Holder(eval(exp))
    case DefAST(pos, name, func) =>
      if (scope.vars contains name)
        problem(pos, s"duplicate declaration: '$name'")

      scope.vars(name) = func
    case exp: ExpressionAST => eval(exp)
  }

  def duplicate(pos: Position, name: String)(implicit scope: Scope): Unit = {
    if (scope.vars contains name)
      problem(pos, s"duplicate declaration: '$name'")
  }

  def beval(exp: ExpressionAST)(implicit scope: Scope) = eval(exp)(scope).asInstanceOf[Boolean]

  def eval(exp: ExpressionAST)(implicit scope: Scope): Any = exp match {
    case BinaryExpressionAST(lpos, left, op, rpos, right) =>
      val l = eval(left)
      val r = eval(right)

      op match {
        case "+" => l.asInstanceOf[Int] + r.asInstanceOf[Int]
      }
    case VariableExpressionAST(pos, name) =>
      scope.vars get name match {
        case None            => problem(pos, s"unknown variable '$name'")
        case Some(Holder(v)) => v
        case Some(v)         => v
      }
    case LiteralExpressionAST(v) => v
    case ListExpressionAST(l)    => l map eval
    case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
      val args1 = args map { case (_, e) => eval(e) }

      eval(f) match {
        case func: (List[Any] => Any) => func(args1.asInstanceOf[List[Any]])
        case FunctionExpressionAST(pos, name, parms, arb, parts, where) =>
          implicit val scope = new Scope
          val alen           = args1.length
          val plen           = parms.length

          if (alen > plen)
            problem(apos, s"too many arguments: expected $plen, found $alen")
          else if (alen < plen)
            problem(apos, s"too few arguments: expected $plen, found $alen")

          args1 zip parms foreach {
            case (a, p) =>
              unify(a, p)
          }

          def testParts(ps: List[FunctionPartAST]): Any =
            ps match {
              case Nil => problem(pos, s"could not apply function: $fpos")
              case h :: t =>
                if (h.guard match {
                      case None    => true
                      case Some(g) => beval(g)
                    })
                  eval(h.body)
                else
                  testParts(t)
            }

          testParts(parts)
      }
  }

  def unify(v: Any, s: PatternAST)(implicit scope: Scope): Boolean =
    s match {
      case VariablePatternAST(pos, name) =>
        duplicate(pos, name)
        scope.vars(name) = v
        true
//      case TuplePatternAST(pos, args) =>
//        v match {
//          case NTuple(elems) =>
//          case _             => false
//        }
    }

  case class Holder(var v: Any)
}

class Scope {
  val vars = new mutable.HashMap[String, Any]
}

case class NTuple(elems: List[Any])
