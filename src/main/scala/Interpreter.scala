package xyz.hyperreal.yola

import scala.collection.mutable
import scala.util.parsing.input.Position

class Interpreter(globalScope: Scope) {

  def apply(ast: AST)(implicit scope: Scope): Any = {
    declarations(ast)
    execute(ast)
  }

  def declarations(ast: AST)(implicit scope: Scope): Any =
    ast match {
      case SourceAST(stmts)           => stmts foreach declarations
      case BlockExpressionAST(stmts)  => stmts foreach declarations
      case DeclarationBlockAST(decls) => decls foreach declarations
      case EnumAST(name, _, _)        =>
      case ValAST(pat, _, _)          =>
      case VarAST(_, name, _)         =>
      case DefAST(pos, name, func)    => implicitly[Scope].addFunctionPiece(pos, name, func)
      case DataAST(_, _, consts)      =>
      case _                          =>
    }

  def execute(ast: AST)(implicit scope: Scope): Value = {
    ast match {
      case SourceAST(stmts) => execute(stmts, scope)
      case DeclarationBlockAST(decls) =>
        decls foreach execute
        YUnit
      case DirectiveBlockAST(dirs) =>
        dirs foreach execute
        YUnit
      case EnumAST(name, pos, enumeration) =>
        var idx = 0

        enumeration foreach {
          case (n, None) =>
            scope.declare(pos, n, Enum(n, idx))
            idx += 1
          case (n, Some(ord)) =>
            scope.declare(pos, n, Enum(n, ord))
            idx = ord + 1
        }

        YUnit
      case ImportAST(module, names) =>
        names foreach {
          case (n, r) =>
            def find(ms: List[String], map: collection.Map[String, Value]): Unit =
              ms match {
                case Nil =>
                  val mod = map.asInstanceOf[Map[String, Value]]

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
                    case Some(m) => find(t, m.asInstanceOf[Map[String, Value]])
                  }
              }

            find(module, globalScope.values)
        }

        YUnit
      case ValAST(pat, pos, expr) =>
        unify(deval(expr), pat, true)
        YUnit
      case VarAST(pos, names, None) =>
        implicitly[Scope].declare(pos, names, Var(YNumber(0)))
        YUnit
      case VarAST(pos, names, Some((_, exp))) =>
        implicitly[Scope].declare(pos, names, Var(deval(exp)))
        YUnit
      case DefAST(pos, names, func) => YUnit
      case DataAST(pos, typ, constructors) =>
        for ((names, fields) <- constructors)
          implicitly[Scope].declare(pos, names, Constructor(typ, names, fields))

        YUnit
      case exp: ExpressionAST => deval(exp)
    }
  }

  def deval(expr: ExpressionAST)(implicit scope: Scope) = deref(eval(expr))

  def leval(expr: ExpressionAST)(implicit scope: Scope) =
    deval(expr).asInstanceOf[YList].v

  def beval(expr: ExpressionAST)(implicit scope: Scope) =
    deval(expr).asInstanceOf[YBoolean].v

  def neval(expr: ExpressionAST)(implicit scope: Scope) =
    deval(expr).asInstanceOf[YNumber].v

  def ieval(expr: ExpressionAST)(implicit scope: Scope) = deval(expr).asInstanceOf(YIterableType)

  def veval(pos: Position, expr: ExpressionAST)(implicit scope: Scope) =
    eval(expr) match {
      case v: Var => v
      case _      => problem(pos, "not an l-value")
    }

  def deref(a: Value) =
    a match {
      case Var(v) => v
      case _      => a
    }

  def execute(l: List[StatementAST], scope: Scope): Value = l match {
    case h :: Nil => execute(h)(scope)
    case h :: t =>
      execute(h)(scope)
      execute(t, scope)
  }

  def eval(expr: ExpressionAST)(implicit scope: Scope): Value = expr match {
    case InterpolationExpressionAST(es) => YString(es map deval map (_.toString) mkString)
    case ComparisonExpressionAST(pos, left, comparisons) =>
      def comp(left: Value, cs: List[(String, Position, ExpressionAST)]): YBoolean =
        cs match {
          case Nil => YBoolean(true)
          case (op, pos, exp) :: t =>
            val right = deval(exp)

            if (op match {
                  case "==" => left == right
                  case "<" =>
                    (left, right) match {
                      case (YString(s), _)          => s < right.toString
                      case (_, YString(s))          => left.toString < s
                      case (YNumber(a), YNumber(b)) => a < b
                    }
                  case ">" =>
                    (left, right) match {
                      case (YString(s), _)          => s > right.toString
                      case (_, YString(s))          => left.toString > s
                      case (YNumber(a), YNumber(b)) => a > b
                    }
                  case "<=" =>
                    (left, right) match {
                      case (YString(s), _)          => s <= right.toString
                      case (_, YString(s))          => left.toString <= s
                      case (YNumber(a), YNumber(b)) => a <= b
                    }
                  case ">=" =>
                    (left, right) match {
                      case (YString(s), _)          => s >= right.toString
                      case (_, YString(s))          => left.toString >= s
                      case (YNumber(a), YNumber(b)) => a >= b
                    }
                })
              comp(right, t)
            else
              YBoolean(false)
        }

      comp(deval(left), comparisons)
    case block @ BlockExpressionAST(stmts) =>
      val inner = new Scope(scope)

      declarations(block)(inner)

      execute(stmts, inner)
    case AssignmentExpressionAST(lhs, op, rhs) =>
      val ll = lhs.length
      val rl = rhs.length

      if (lhs.length > rhs.length)
        problem(lhs.head._1, s"left hand side has too many items: l.h.s. has $ll, r.h.s has $rl")

      if (lhs.length < rhs.length)
        problem(lhs.head._1, s"right hand side has too many items: l.h.s. has $ll, r.h.s has $rl")

      YList((lhs zip rhs.map { case (pr, er) => eval(er) }) map {
        case ((pl, el), v) => {
          val h = veval(pl, el)

          op match {
            case "=" => h.v = v
            case "+=" =>
              h.v = h.v match {
                case s: YString => s append String.valueOf(v)
                case n: YNumber => n + v.asInstanceOf[YNumber].v
              }
            case "-=" => h.v = h.v.asInstanceOf[YNumber] - v.asInstanceOf[YNumber].v
            case "*=" =>
              h.v = YNumber(h.v.asInstanceOf[YNumber].v * v.asInstanceOf[YNumber].v)
            case "/=" =>
              h.v = YNumber(h.v.asInstanceOf[YNumber].v / v.asInstanceOf[YNumber].v)
          }

          h.v
        }
      })
    case UnaryExpressionAST("-", _, expr) => YNumber(-neval(expr))
    case PostExpressionAST(op, pos, expr) =>
      val h = veval(pos, expr)

      op match {
        case "++" =>
          val res = h.v

          h.v = h.v.asInstanceOf[YNumber] + 1
          res
        case "--" =>
          val res = h.v

          h.v = h.v.asInstanceOf[YNumber] - 1
          res
      }
    case PreExpressionAST(op, pos, expr) =>
      val h = veval(pos, expr)

      op match {
        case "++" =>
          h.v = h.v.asInstanceOf[YNumber] + 1
          h.v
        case "--" =>
          h.v = h.v.asInstanceOf[YNumber] - 1
          h.v
      }
    case RepeatExpressionAST(label, body) =>
      def repeatLoop: Value = {
        try {
          try {
            eval(body)
          } catch {
            case ContinueException(_, clabel) if clabel.isEmpty || clabel == label =>
          }
        } catch {
          case BreakException(_, blabel, expr) if blabel.isEmpty || blabel == label =>
            return expr map deval getOrElse YUnit
        }

        repeatLoop
      }

      repeatLoop
    case BreakExpressionAST(pos, label, expr) => throw BreakException(pos, label, expr)
    case ContinueExpressionAST(pos, label)    => throw ContinueException(pos, label)
    case WhileExpressionAST(label, cond, body, els) =>
      def whileLoop: Value = {
        try {
          try {
            if (beval(cond))
              body foreach deval
            else
              return els map deval getOrElse YUnit
          } catch {
            case ContinueException(_, clabel) if clabel.isEmpty || clabel == label =>
          }
        } catch {
          case BreakException(_, blabel, expr) if blabel.isEmpty || blabel == label =>
            return expr map deval getOrElse YUnit
        }

        whileLoop
      }

      whileLoop
    case ForYieldExpressionAST(gen, body)          => YIterable(flatMap(gen, scope, body))
    case ListComprehensionExpressionAST(expr, gen) => YList(flatMap(gen, scope, expr).toList)
    case ForExpressionAST(label, gen, body, els) =>
      def foreach(gs: List[GeneratorExpressionAST], outer: Scope): Value =
        gs match {
          case Nil => deval(body)(outer)
          case GeneratorExpressionAST(pattern, pos, iterable, filter) :: tail =>
            ieval(iterable).iterator.foreach(v => {
              try {
                try {
                  val inner = new Scope(outer)

                  unify(v, pattern, true)(inner)

                  if (!filter.isDefined || beval(filter.get)(inner))
                    foreach(tail, inner)
                } catch {
                  case ContinueException(_, clabel) if clabel.isEmpty || clabel == label =>
                }
              } catch {
                case BreakException(_, blabel, expr) if blabel.isEmpty || blabel == label =>
                  return expr map deval getOrElse YUnit
              }
            })

            els map deval getOrElse YUnit
        }

      foreach(gen, scope)
    case ConditionalExpressionAST(cond, els) =>
      deval(
        cond find { case (c, _) => beval(c) } map { case (_, a) => a } getOrElse (els getOrElse LiteralExpressionAST(
          ()
        ))
      )
    case DotExpressionAST(epos, expr, apos, field) =>
      deval(expr) match {
        case f: (Value => Value) => f(YString(field))
      }
    case BinaryExpressionAST(lpos, left, op, rpos, right) =>
      val l = deval(left)
      val r = deval(right)

      (l, r) match {
        case (YNumber(a), YNumber(b)) =>
          op match {
            case "+"         => YNumber(a + b)
            case "-"         => YNumber(a - b)
            case "/"         => YNumber(a / b)
            case "%"         => YNumber(a % b)
            case "*" | "adj" => YNumber(a * b)
          }
        case (s: YString, _) if op == "+" => s append r.toString
        case (_, s: YString) if op == "+" => s prepend l.toString
      }
    case VariableExpressionAST(pos, names) =>
      scope get names match {
        case None    => problem(pos, s"undeclared variable '$names'")
        case Some(v) => v
      }
    case LiteralExpressionAST(())            => YUnit
    case LiteralExpressionAST(n: BigDecimal) => YNumber(n)
    case LiteralExpressionAST(s: String)     => YString(s)
    case LiteralExpressionAST(b: Boolean)    => YBoolean(b)
    case LiteralExpressionAST(null)          => YNull
    case TupleExpressionAST(elems)           => YTuple(elems map deval)
    case ListExpressionAST(l)                => YList(l map deval)
    case MapExpressionAST(entries) =>
      YMap(entries map {
        case (k, v) =>
          (k match {
            case VariableExpressionAST(_, key) => YString(key)
            case _                             => deval(k)
          }) -> deval(v)
      } toMap)
    case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
      call(fpos, deval(f), apos, args map { case (_, e) => deval(e) })
    case ConsExpressionAST(lpos, left, rpos, right) =>
      YList(deval(left) :: leval(right))
    case RangeExpressionAST(fpos, from, tpos, to, bpos, by, incl) =>
      YRange(neval(from), neval(to), neval(by), incl)
    case AndExpressionAST(left, right) => YBoolean(beval(left) && beval(right))
    case OrExpressionAST(left, right)  => YBoolean(beval(left) || beval(right))
    case NotExpressionAST(cond)        => YBoolean(!beval(cond))
    case f: FunctionExpressionAST =>
      if (f.scope eq null)
        f.scope = scope

      f
  }

  def flatMap(
      gs: List[GeneratorExpressionAST],
      outer: Scope,
      expr: ExpressionAST
  ): collection.Iterable[Value] =
    gs match {
      case Nil => List(deval(expr)(outer))
      case GeneratorExpressionAST(pattern, pos, iterable, filter) :: tail =>
        ieval(iterable)(outer).iterator.toList.flatMap(v => {
          val inner = new Scope(outer)

          unify(v, pattern, true)(inner)

          if (!filter.isDefined || beval(filter.get)(inner))
            flatMap(tail, inner, expr)
          else
            Nil
        })
    }

  def call(fpos: Position, f: Any, apos: Position, args: List[Value]): Value =
    f match {
      case NativeFunction(func) => func(args)
      case Constructor(_, names, Nil) =>
        problem(fpos, "nullary constructors can't be applied")
      case con @ Constructor(typ, names, fields) =>
        if (fields.length != args.length)
          problem(
            apos,
            s"wrong number of arguments for constructor '$names': got ${args.length}, expected ${fields.length}"
          )

        Record(con, mutable.LinkedHashMap(fields zip args: _*))
      case Functions(map) =>
        map get args.length match {
          case None    => problem(fpos, s"function of arity ${args.length} not found")
          case Some(f) => call(fpos, f, apos, args)
        }
      case fexp @ FunctionExpressionAST(pieces) =>
        pieces.head match {
          case FunctionPieceAST(pos, parms, arb, parts, where) =>
            val alen = args.length
            val plen = parms.length

            if (alen > plen)
              problem(apos, s"too many arguments: expected $plen, found $alen")
            else if (alen < plen)
              problem(apos, s"too few arguments: expected $plen, found $alen")
        }

        def testPieces(ps: List[FunctionPieceAST]): Value =
          ps match {
            case Nil => problem(fpos, s"could not find matching function piece")
            case FunctionPieceAST(pos, parms, arb, parts, where) :: t =>
              implicit val scope = new Scope(fexp.scope)

              if (args zip parms forall { case (a, p) => unify(a, p, false) }) {
                def testParts(ps: List[FunctionPart]): Value =
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

                where foreach apply
                testParts(parts)
              } else
                testPieces(t)
          }

        testPieces(pieces)
    }

  def unify(v: Value, s: PatternAST, errors: Boolean)(implicit scope: Scope): Boolean =
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
          case YMap(m) =>
            val keySet = m.keySet.asInstanceOf[Set[String]]

            if (entries subsetOf keySet) {
              for (e <- entries)
                implicitly[Scope].declare(null, e, m.asInstanceOf[Map[String, Value]](e))

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
      case VariablePatternAST(pos, names) =>
        implicitly[Scope].declare(pos, names, v)
        true
      case ListPatternAST(pos, elems) =>
        v match {
          case YList(l) =>
            def unifyList(l: List[Value], elems: List[PatternAST]): Boolean =
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
          case YList(h :: t) =>
            unify(h, head, errors) && unify(YList(t), tail, errors)
          case _ =>
            if (errors)
              problem(pos, "expected list")
            else
              false
        }
      case RecordPatternAST(pos, names, args) =>
        v match {
          case Record(Constructor(_, rname, _), rargs)
              if names == rname && args.length == rargs.size =>
            rargs.values zip args forall { case (e, a) => unify(e, a, errors) }
//          case p: Product if names == p.productPrefix && args.length == p.productArity =>
//            p.productIterator.toList zip args forall { case (e, a) => unify(e, a, errors) }
          case _ =>
            if (errors)
              problem(pos, s"expected record '$names'")
            else
              false
        }
      case TuplePatternAST(pos, elems) =>
        v match {
          case YTuple(elems2) =>
            if (elems.length != elems2.length)
              if (errors)
                problem(pos, "tuple has wrong number of elems")
              else
                false
            else
              elems2 zip elems forall { case (e, a) => unify(e, a, errors) }
          // todo: Product destructuring
//          case p: Product
//              if p.productPrefix == s"Tuple${elems.length}" && elems.length == p.productArity =>
//            p.productIterator.toList zip elems forall { case (e, a) => unify(e, a, errors) }
          case _ =>
            if (errors)
              problem(pos, "expected tuple")
            else
              false
        }
    }
}

case class Var(var v: Value) extends Value(v.containing, null) // todo: var typing

case class BreakException(pos: Position, blabel: Option[String], expr: Option[ExpressionAST])
    extends RuntimeException

case class ContinueException(pos: Position, clabel: Option[String]) extends RuntimeException
