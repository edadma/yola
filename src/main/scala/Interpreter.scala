package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

object Interpreter {

  def apply(ast: AST)(implicit scope: Scope): Any = ast match {
    case DeclarationBlockAST(decls) =>
      decls map apply
      ()
    case ValAST(pat, pos, expr) =>
      unify(deval(expr), pat, true)
    case VarAST(pos, name, None) =>
      declare(pos, name, Var(0))
    case VarAST(pos, name, Some((_, exp))) =>
      declare(pos, name, Var(deval(exp)))
    case DefAST(pos, name, func) =>
      duplicate(pos, name)
      func.scope = scope
      scope.vars(name) = func
    case DataAST(pos, name, constructors) =>
    case exp: ExpressionAST               => deval(exp)
  }

  def duplicate(pos: Position, name: String)(implicit scope: Scope): Unit = {
    if (scope.vars contains name)
      problem(pos, s"duplicate declaration: '$name'")
  }

  def deval(expr: ExpressionAST)(implicit scope: Scope) = deref(eval(expr))

  def beval(expr: ExpressionAST)(implicit scope: Scope) = deval(expr).asInstanceOf[Boolean]

  def ieval(expr: ExpressionAST)(implicit scope: Scope) = deval(expr).asInstanceOf[Iterable[Any]]

  def deref(a: Any) =
    a match {
      case Var(v) => v
      case _      => a
    }

  def eval(expr: ExpressionAST)(implicit scope: Scope): Any = expr match {
    case SectionExpressionAST(op)       =>
    case InterpolationExpressionAST(es) => es map deval map display mkString
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
      val inner = new Scope(scope)

      def evals(l: List[StatementAST]): Any = l match {
        case h :: Nil => apply(h)(inner)
        case h :: t =>
          apply(h)(inner)
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
            case h: Var =>
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
        case h: Var =>
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
        case _ => problem(pos, "not an l-value")
      }
    case RepeatExpressionAST(label, body) =>
      while (true) eval(body)
    case WhileExpressionAST(label, cond, body, els) =>
      while (beval(cond)) {
        body foreach eval
      }

      els foreach eval
    case ForYieldExpressionAST(gen, body) =>
      def flatMap(gs: List[GeneratorExpressionAST], outer: Scope): collection.Iterable[Any] =
        gs match {
          case Nil => List(deval(body)(outer))
          case GeneratorExpressionAST(pattern, pos, iterable, filter) :: tail =>
            ieval(iterable).flatMap(v => {
              val inner = new Scope(outer)

              unify(v, pattern, true)(inner)

              if (!filter.isDefined || beval(filter.get)(inner))
                flatMap(tail, inner)
              else
                Nil
            })
        }

      flatMap(gen, scope)
    case ForExpressionAST(label, gen, body, els) =>
      def foreach(gs: List[GeneratorExpressionAST], outer: Scope): Unit =
        gs match {
          case Nil => deval(body)(outer)
          case GeneratorExpressionAST(pattern, pos, iterable, filter) :: tail =>
            ieval(iterable).foreach(v => {
              val inner = new Scope(outer)

              unify(v, pattern, true)(inner)

              if (!filter.isDefined || beval(filter.get)(inner))
                foreach(tail, inner)
            })
        }

      foreach(gen, scope)
      els foreach eval
    case ConditionalExpressionAST(cond, els) =>
      deval(
        cond find { case (c, _) => beval(c) } map { case (_, a) => a } getOrElse (els getOrElse LiteralExpressionAST(
          ()
        ))
      )
    case DotExpressionAST(epos, expr, apos, field) =>
      deval(expr) match {
        case f: (Any => Any) => f(field)
      }
    case BinaryExpressionAST(lpos, left, op, rpos, right) =>
      val l = deval(left)
      val r = deval(right)

      op match {
        case "+" => l.asInstanceOf[Int] + r.asInstanceOf[Int]
        case "%" => l.asInstanceOf[Int] % r.asInstanceOf[Int]
      }
    case VariableExpressionAST(pos, name) =>
      scope get name match {
        case None    => problem(pos, s"undeclared variable '$name'")
        case Some(v) => v
      }
    case LiteralExpressionAST(v)   => v
    case TupleExpressionAST(elems) => NTuple(elems map deval)
    case ListExpressionAST(l)      => l map deval
    case MapExpressionAST(entries) =>
      entries map {
        case (k, v) =>
          (k match {
            case VariableExpressionAST(_, key) => key
            case _                             => deval(k)
          }) -> deval(v)
      } toMap
    case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
      call(fpos, deval(f), apos, args map { case (_, e) => deval(e) })
    case ConsExpressionAST(lpos, left, rpos, right) =>
      deval(left) :: deval(right).asInstanceOf[List[Any]]
    case RangeExpressionAST(fpos, from, tpos, to, bpos, by, incl) =>
      val start = deval(from).asInstanceOf[Int]
      val end   = deval(to).asInstanceOf[Int]
      val step  = deval(by).asInstanceOf[Int]

      if (incl)
        start to end by step
      else
        start until end by step
    case AndExpressionAST(left, right) => beval(left) && beval(right)
    case OrExpressionAST(left, right)  => beval(left) || beval(right)
    case NotExpressionAST(cond)        => !beval(cond)
    case f: FunctionExpressionAST      => f
  }

  def call(fpos: Position, f: Any, apos: Position, args: List[Any]) =
    f match {
      case func: (List[Any] => Any) => func(args)
      case f @ FunctionExpressionAST(pos, name, parms, arb, parts, where) =>
        implicit val scope = new Scope(f.scope)
        val alen           = args.length
        val plen           = parms.length

        if (alen > plen)
          problem(apos, s"too many arguments: expected $plen, found $alen")
        else if (alen < plen)
          problem(apos, s"too few arguments: expected $plen, found $alen")

        args zip parms foreach {
          case (a, p) =>
            unify(a, p, false)
        }

        def testParts(ps: List[FunctionPart]): Any =
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

  def declare(pos: Position, name: String, value: Any)(implicit scope: Scope) = {
    duplicate(pos, name)
    scope.vars(name) = value
  }

  def unify(v: Any, s: PatternAST, errors: Boolean)(implicit scope: Scope): Boolean =
    s match {
      case AlternationPatternAST(pos, alts) =>
        def alternate(as: List[PatternAST]): Boolean =
          as match {
            case Nil =>
              if (errors)
                problem(pos, "none of the alternatives match")
              else
                false
            case h :: t =>
              if (unify(v, h, false))
                true
              else
                alternate(t)
          }

        alternate(alts)
      case MapPatternAST(pos, entries) =>
        v match {
          case m: collection.Map[_, _] =>
            val keySet = m.keySet.asInstanceOf[Set[String]]

            if (entries subsetOf keySet) {
              for (e <- entries)
                declare(null, e, m.asInstanceOf[Map[String, Any]](e))
              true
            } else if (errors)
              problem(
                pos,
                s"missing entry: ${entries diff (entries intersect keySet) mkString ", "}"
              )
            else
              false
          case _ =>
            if (errors)
              problem(pos, "expected map")
            else
              false
        }
      case LiteralPatternAST(pos, lit) =>
        if (v != lit)
          if (errors)
            problem(pos, "literal doesn't match")
          else
            false
        else
          true
      case VariablePatternAST(pos, name) =>
        declare(pos, name, v)
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
      case NamedPatternAST(pos, alias, pat) =>
        if (unify(v, pat, errors)) {
          declare(pos, alias, v)
          true
        } else
          false
      case ConsPatternAST(pos, head, tail) =>
        v match {
          case h :: t =>
            unify(h, head, errors) && unify(t, tail, errors)
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

  case class Var(var v: Any)
}

class Scope(val outer: Scope) {
  val vars = new mutable.HashMap[String, Any]

  def get(name: String): Option[Any] =
    vars get name orElse (if (outer eq null) None else outer.get(name))
}

case class NTuple(elems: List[Any])

case class Constructor(typ: String, name: String, fields: List[String])

case class Record(typ: String, name: String, fields: List[String], args: List[Any])
