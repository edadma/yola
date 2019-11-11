package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

object YolaInterpreter {

  def apply(ast: AST)(implicit scope: Scope): Any = ast match {
    case SourceAST(stmts)           => stmts foreach apply
    case DeclarationBlockAST(decls) => decls map apply
    case ValAST(pat, pos, expr)     => unify(deval(expr), pat, true)
    case VarAST(pos, name, None) =>
      duplicate(pos, name)
      scope.vars(name) = Holder(0)
    case VarAST(pos, name, Some((pose, exp))) =>
      duplicate(pos, name)
      scope.vars(name) = Holder(deval(exp))
    case DefAST(pos, name, func) =>
      duplicate(pos, name)
      func.scope = scope
      scope.vars(name) = func
    case exp: ExpressionAST => deval(exp)
  }

  def duplicate(pos: Position, name: String)(implicit scope: Scope): Unit = {
    if (scope.vars contains name)
      problem(pos, s"duplicate declaration: '$name'")
  }

  def deval(expr: ExpressionAST)(implicit scope: Scope) = deref(eval(expr))

  def beval(expr: ExpressionAST)(implicit scope: Scope) = deval(expr).asInstanceOf[Boolean]

  def deref(a: Any) =
    a match {
      case Holder(v) => v
      case _         => a
    }

  def eval(expr: ExpressionAST)(implicit scope: Scope): Any = expr match {
    case ComparisonExpressionAST(pos, left, comparisons) =>
      def comp(left: Any, cs: List[(String, Position, ExpressionAST)]): Boolean =
        cs match {
          case Nil => true
          case (op, pos, exp) :: t =>
            val right = deval(exp)

            if (op match {
                  case "==" => left == right
                  case "<"  => left.asInstanceOf[Int] < right.asInstanceOf[Int]
                  case ">"  => left.asInstanceOf[Int] > right.asInstanceOf[Int]
                  case "<=" => left.asInstanceOf[Int] <= right.asInstanceOf[Int]
                  case ">=" => left.asInstanceOf[Int] >= right.asInstanceOf[Int]
                })
              comp(right, t)
            else
              false
        }

      comp(deval(left), comparisons)
    case BlockExpressionAST(stmts) =>
      def evals(l: List[StatementAST]): Any = l match {
        case h :: Nil => apply(h)
        case h :: t =>
          apply(h)
          evals(t)
      }

      evals(stmts)
    case AssignmentExpressionAST(lhs, op, rhs) =>
      val ll = lhs.length
      val rl = rhs.length

      if (lhs.length > rhs.length)
        problem(lhs.head._1, s"left hand side has too many items: l.h.s. has $ll, r.h.s has $rl")

      if (lhs.length < rhs.length)
        problem(lhs.head._1, s"right hand side has too many items: l.h.s. has $ll, r.h.s has $rl")

      (lhs zip rhs.map { case (pr, er) => eval(er) }) map {
        case ((pl, el), v) =>
          eval(el) match {
            case h: Holder =>
              op match {
                case "="  => h.v = v
                case "+=" => h.v = h.v.asInstanceOf[Int] + v.asInstanceOf[Int]
              }
              h.v
            case _ => problem(pl, "not an l-value")
          }
      }
    case UnaryExpressionAST(op, pos, expr) =>
      eval(expr) match {
        case h: Holder =>
          op match {
            case "_++" =>
              val res = h.v
              h.v = h.v.asInstanceOf[Int] + 1
              res
            case "_--" =>
              val res = h.v
              h.v = h.v.asInstanceOf[Int] - 1
              res
            case "++_" =>
              h.v = h.v.asInstanceOf[Int] + 1
              h.v
            case "--_" =>
              h.v = h.v.asInstanceOf[Int] - 1
              h.v
          }
        case _ => problem(pos, "not al l-value")
      }
    case WhileExpressionAST(label, cond, body, els) =>
      while (beval(cond)) body foreach eval
      els foreach eval
    case ConditionalExpressionAST(cond, els) =>
      eval(
        cond find { case (c, _) => beval(c) } map { case (_, a) => a } getOrElse (els getOrElse LiteralExpressionAST(
          ())))
    case DotExpressionAST(epos, expr, apos, field) =>
      eval(expr) match {
        case f: (Any => Any) => f(field)
      }
    case BinaryExpressionAST(lpos, left, op, rpos, right) =>
      val l = deval(left)
      val r = deval(right)

      op match {
        case "+" => l.asInstanceOf[Int] + r.asInstanceOf[Int]
      }
    case VariableExpressionAST(pos, name) =>
      scope get name match {
        case None    => problem(pos, s"unknown variable '$name'")
        case Some(v) => v
      }
    case LiteralExpressionAST(v) => v
    case TupleExpressionAST(l)   => NTuple(l map deval)
    case ListExpressionAST(l)    => l map deval
    case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
      val args1 = args map { case (_, e) => deval(e) }

      deval(f) match {
        case func: (List[Any] => Any) => func(args1)
        case f @ FunctionAST(pos, name, parms, arb, parts, where) =>
          implicit val scope = new Scope(f.scope)
          val alen           = args1.length
          val plen           = parms.length

          if (alen > plen)
            problem(apos, s"too many arguments: expected $plen, found $alen")
          else if (alen < plen)
            problem(apos, s"too few arguments: expected $plen, found $alen")

          args1 zip parms foreach {
            case (a, p) =>
              unify(a, p, false)
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

  def unify(v: Any, s: PatternAST, errors: Boolean)(implicit scope: Scope): Boolean =
    s match {
      case VariablePatternAST(pos, name) =>
        duplicate(pos, name)
        scope.vars(name) = v
        true
      case ListPatternAST(pos, elems) =>
        v match {
          case l: List[_] =>
            def unifyList(l: List[_], elems: List[PatternAST]): Boolean =
              (l, elems) match {
                case (Nil, Nil)         => true
                case (h :: t, eh :: et) => unify(h, eh, errors) && unifyList(t, et)
                case _ =>
                  if (errors)
                    problem(pos, "wrong number of elements in list")
                  else
                    false
              }

            unifyList(l, elems)
          case _ =>
            if (errors)
              problem(pos, "expected list")
            else
              false
        }
      case TuplePatternAST(pos, elems) =>
        v match {
          case NTuple(elems2) =>
            if (elems.length != elems2.length)
              if (errors)
                problem(pos, "tuple has wrong number of elems")
              else
                false
            else
              elems2 zip elems forall { case (e, a) => unify(e, a, errors) }
          case _ =>
            if (errors)
              problem(pos, "expected tuple")
            else
              false
        }
    }

  case class Holder(var v: Any)
}

class Scope(val outer: Scope) {
  val vars = new mutable.HashMap[String, Any]

  def get(name: String): Option[Any] =
    vars get name orElse (if (outer eq null)
                            None
                          else
                            outer.get(name))
}

case class NTuple(elems: List[Any])
