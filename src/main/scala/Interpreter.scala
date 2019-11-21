package xyz.hyperreal.yola

import scala.util.parsing.input.Position

class Interpreter(loader: (List[String], String, Option[String], Scope) => Unit) {

  def apply(ast: AST)(implicit scope: Scope): Any = ast match {
    case DeclarationBlockAST(decls) =>
      decls map apply
      ()
    case EnumAST(name, pos, enumeration) =>
      var idx = 0

      enumeration foreach {
        case (name, None) =>
          scope.declare(pos, name, Enum(name, idx))
          idx += 1
        case (name, Some(ord)) =>
          scope.declare(pos, name, Enum(name, ord))
          idx = ord + 1
      }
    case ImportAST(module, names) =>
      names foreach {
        case (n, r) =>
          scope get module.head match {
            case None => loader(module, n, r, scope)
            case Some(m: Map[String, Any]) =>
              def find(ms: List[String], map: Map[String, Any]): Unit =
                ms match {
                  case Nil =>
                    val mod = map.asInstanceOf[Map[String, List[Any] => Any]]

                    if (n == "_")
                      for ((k, v) <- mod)
                        scope.declare(null, k, v)
                    else
                      mod get n match {
                        case None => perror(s"member '$n' not found")
                        case Some(o) =>
                          val mem =
                            r match {
                              case None          => n
                              case Some(newname) => newname
                            }

                          scope.declare(null, mem, o)
                      }
                  case h :: t =>
                    map get h match {
                      case None    => perror(s"module '$h' not found")
                      case Some(m) => find(t, m.asInstanceOf[Map[String, Any]])
                    }
                }

              find(module.tail, m)
          }
      }
    case ValAST(pat, pos, expr)            => unify(deval(expr), pat, true)
    case VarAST(pos, name, None)           => implicitly[Scope].declare(pos, name, Var(0))
    case VarAST(pos, name, Some((_, exp))) => implicitly[Scope].declare(pos, name, Var(deval(exp)))
    case DefAST(pos, name, func) =>
      func.scope = scope
      implicitly[Scope].add(pos, name, func)
    case DataAST(pos, typ, constructors) =>
      for ((name, fields) <- constructors)
        implicitly[Scope].declare(pos, name, Constructor(typ, name, fields))
    case exp: ExpressionAST => deval(exp)
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
    case ForYieldExpressionAST(gen, body)          => flatMap(gen, scope, body)
    case ListComprehensionExpressionAST(expr, gen) => flatMap(gen, scope, expr).toList
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
        case "+"         => l.asInstanceOf[Int] + r.asInstanceOf[Int]
        case "%"         => l.asInstanceOf[Int] % r.asInstanceOf[Int]
        case "*" | "adj" => l.asInstanceOf[Int] * r.asInstanceOf[Int]
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

  def flatMap(
      gs: List[GeneratorExpressionAST],
      outer: Scope,
      expr: ExpressionAST
  ): collection.Iterable[Any] =
    gs match {
      case Nil => List(deval(expr)(outer))
      case GeneratorExpressionAST(pattern, pos, iterable, filter) :: tail =>
        ieval(iterable)(outer).flatMap(v => {
          val inner = new Scope(outer)

          unify(v, pattern, true)(inner)

          if (!filter.isDefined || beval(filter.get)(inner))
            flatMap(tail, inner, expr)
          else
            Nil
        })
    }

  def call(fpos: Position, f: Any, apos: Position, args: List[Any]): Any =
    f match {
      case func: (List[Any] => Any) => func(args)
      case Constructor(_, name, Nil) =>
        problem(fpos, "nullary constructors can't be applied")
      case con @ Constructor(typ, name, fields) =>
        if (fields.length != args.length)
          problem(
            apos,
            s"wrong number of arguments for constructor '$name': got ${args.length}, expected ${fields.length}"
          )

        Record(con, args)
      case Functions(map) =>
        map get args.length match {
          case None    => problem(fpos, s"function of arity ${args.length} not found")
          case Some(f) => call(fpos, f, apos, args)
        }
      case FunctionExpressionAST(pieces) =>
        pieces.head match {
          case FunctionPieceAST(pos, parms, arb, parts, where) =>
            val alen = args.length
            val plen = parms.length

            if (alen > plen)
              problem(apos, s"too many arguments: expected $plen, found $alen")
            else if (alen < plen)
              problem(apos, s"too few arguments: expected $plen, found $alen")
        }

        def testPieces(ps: List[FunctionPieceAST]): Any =
          ps match {
            case Nil => problem(fpos, s"could not find matching function piece")
            case (piece @ FunctionPieceAST(pos, parms, arb, parts, where)) :: t =>
              implicit val scope = new Scope(piece.scope)

              if (args zip parms forall { case (a, p) => unify(a, p, false) }) {
                def testParts(ps: List[FunctionPart]): Any =
                  ps match {
                    case Nil => problem(pos, s"could not apply function")
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
              } else
                testPieces(t)
          }

        testPieces(pieces)
    }
//      case f @ FunctionPieceAST(pos, parms, arb, parts, where) =>
//        implicit val scope = new Scope(f.scope)
//        val alen           = args.length
//        val plen           = parms.length
//
//        if (alen > plen)
//          problem(apos, s"too many arguments: expected $plen, found $alen")
//        else if (alen < plen)
//          problem(apos, s"too few arguments: expected $plen, found $alen")
//
//        args zip parms foreach {
//          case (a, p) =>
//            unify(a, p, false)
//        }
//
//        def testParts(ps: List[FunctionPart]): Any =
//          ps match {
//            case Nil => problem(pos, s"could not apply function: $fpos")
//            case h :: t =>
//              if (h.guard match {
//                    case None    => true
//                    case Some(g) => beval(g)
//                  })
//                eval(h.body)
//              else
//                testParts(t)
//          }
//
//        testParts(parts)

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
                implicitly[Scope].declare(null, e, m.asInstanceOf[Map[String, Any]](e))
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
        implicitly[Scope].declare(pos, name, v)
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
          implicitly[Scope].declare(pos, alias, v)
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
      case RecordPatternAST(pos, name, args) =>
        v match {
          case Record(Constructor(_, rname, _), rargs)
              if name == rname && args.length == rargs.length =>
            rargs zip args forall { case (e, a) => unify(e, a, errors) }
          case _ =>
            if (errors)
              problem(pos, s"expected record '$name'")
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

case class NTuple(elems: List[Any])

case class Constructor(typ: String, name: String, fields: List[String])

case class Record(con: Constructor, args: List[Any])

case class Enum(name: String, ordinal: Int) extends (Any => Any) {

  def apply(v: Any) =
    v match {
      case "name"    => name
      case "ordinal" => ordinal
    }
}
