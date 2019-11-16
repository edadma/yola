package xyz.hyperreal.yola

import scala.collection.mutable.ListBuffer

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.CharArrayReader.EofCh
import util.parsing.input.{CharSequenceReader, Positional, Reader}

import xyz.hyperreal.indentation_lexical_native._

object Interpolation {
  val INTERPOLATION_PATTERN    = """\$(?:([a-zA-Z_]+\d*)|\{([^}]+)\}|\$)""" r
  val INTERPOLATED_PATTERN     = """[\ue000-\ue002]([^\ue000-\ue002]+)""" r
  val INTERPOLATION_DELIMITER  = '\ue000'
  val INTERPOLATION_LITERAL    = '\ue000'
  val INTERPOLATION_VARIABLE   = '\ue001'
  val INTERPOLATION_EXPRESSION = '\ue002'
}

class YolaLexical
    extends IndentationLexical(false,
                               true,
                               List("{", "[", "("),
                               List("}", "]", ")"),
                               "//",
                               "/*",
                               "*/") {

  import Interpolation._

  override def token: Parser[Token] = regexToken | stringToken | decimalToken | super.token

  override def identChar = letter | elem('_') // | elem('$')

  override def whitespace: Parser[Any] = rep[Any](
    whitespaceChar
      | '/' ~ '*' ~ comment
      | '/' ~ '/' ~ rep(chrExcept(EofCh, '\n'))
      | '/' ~ '*' ~ failure("unclosed comment")
  )

  case class RegexLit(chars: String) extends Token

  private def regexToken: Parser[Token] =
    '`' ~> rep(guard(not('`')) ~> (('\\' ~ '`' ^^^ "\\`") | elem("", ch => true))) <~ '`' ^^ { l =>
      RegexLit(l mkString)
    }

  private def stringToken: Parser[Token] =
    ('\'' ~ '\'' ~ '\'') ~> rep(guard(not('\'' ~ '\'' ~ '\'')) ~> elem("", ch => true)) <~ ('\'' ~ '\'' ~ '\'') ^^ {
      l =>
        StringLit(l mkString)
    } |
      ('"' ~ '"' ~ '"') ~> rep(guard(not('"' ~ '"' ~ '"')) ~> elem("", ch => true)) <~ ('"' ~ '"' ~ '"') ^^ {
        l =>
          StringLit(interpolate(l mkString, false))
      } |
      '\'' ~> rep(guard(not('\'')) ~> (('\\' ~ '\'' ^^^ "\\'") | elem("", ch => true))) <~ '\'' ^^ {
        l =>
          StringLit(escape(l mkString))
      } |
      '"' ~> rep(guard(not('"')) ~> (('\\' ~ '"' ^^^ "\\\"") | elem("", ch => true))) <~ '"' ^^ {
        l =>
          StringLit(interpolate(l mkString, true))
      }

  private def escape(s: String) = {
    val buf = new StringBuilder

    def chr(r: Reader[Char]): Unit = {
      if (!r.atEnd) {
        if (r.first == '\\') {
          if (r.rest.atEnd)
            sys.error("unexpected end of string") //todo: nicer error reporting; not easy - will have to return a special "error" object

          if (r.rest.first == 'u') {
            var u = r.rest.rest

            def nextc =
              if (u.atEnd)
                sys.error("unexpected end of string inside unicode sequence")
              else {
                val res = u.first

                u = u.rest
                res
              }

            buf append Integer.valueOf(new String(Array(nextc, nextc, nextc, nextc)), 16).toChar
            chr(u)
          } else {
            buf.append(
              Map(
                '\\' -> '\\',
                '\'' -> '\'',
                '"'  -> '"',
                '$'  -> '$',
                '/'  -> '/',
                'b'  -> '\b',
                'f'  -> '\f',
                'n'  -> '\n',
                'r'  -> '\r',
                't'  -> '\t'
              ).get(r.rest.first) match {
                case Some(c) => c
                case _       => sys.error("illegal escape character " + r.rest.first)
              })

            chr(r.rest.rest)
          }
        } else {
          buf append r.first
          chr(r.rest)
        }
      }
    }

    chr(new CharSequenceReader(s))
    buf.toString()
  }

  private def interpolate(s: String, handleEscape: Boolean): String = {
    val buf        = new StringBuilder
    var last       = 0
    var nonliteral = false

    def append(code: Char, s: String): Unit = {
      buf += code
      buf append s
    }

    def literal(s: String) = append(INTERPOLATION_LITERAL, if (handleEscape) escape(s) else s)

    for (m <- INTERPOLATION_PATTERN.findAllMatchIn(s)) {
      if (m.start > last)
        literal(s.substring(last, m.start))

      m.matched.charAt(1) match {
        case '$' => literal("$")
        case '{' => append(INTERPOLATION_EXPRESSION, m.group(2))
        case _   => append(INTERPOLATION_VARIABLE, m.group(1))
      }

      nonliteral = true
      last = m.end
    }

    if (last < s.length)
      literal(s.substring(last))

    if (!nonliteral)
      buf.deleteCharAt(0)

    buf.toString
  }

  private def decimalToken: Parser[Token] =
    digits ~ '.' ~ digits ~ optExponent ^^ {
      case intPart ~ _ ~ fracPart ~ exp => NumericLit(s"$intPart.$fracPart$exp")
    } |
      '.' ~ digits ~ optExponent ^^ { case _ ~ fracPart ~ exp => NumericLit(s".$fracPart$exp") } |
      digits ~ optExponent ^^ { case intPart ~ exp            => NumericLit(s"$intPart$exp") }

  private def digits = rep1(digit) ^^ (_ mkString)

  private def chr(c: Char) = elem("", ch => ch == c)

  private def exponent = (chr('e') | 'E') ~ opt(chr('+') | '-') ~ digits ^^ {
    case e ~ None ~ exp    => List(e, exp) mkString
    case e ~ Some(s) ~ exp => List(e, s, exp) mkString
  }

  private def optExponent = opt(exponent) ^^ {
    case None    => ""
    case Some(e) => e
  }

  reserved ++= List(
    "if",
    "then",
    "else",
    "elif",
    "every",
    "for",
    "while",
    "break",
    "continue",
    "return",
    "do",
    "yield",
    "repeat",
    "by",
    "or",
    "and",
    "is",
    "not",
    "div",
    "mod",
    "to",
    "until",
    "where",
    "def",
    "var",
    "val",
    "data",
    "otherwise",
    "module", //todo: implement module system similar to Modula
    "null",
    "true",
    "false"
  )

  delimiters ++= List(
    "+",
    "*",
    "-",
    "/",
    "\\",
    "%",
    "^",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    ",",
    "=",
    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">=",
    ":",
    "->",
    ".",
    ";",
    "!",
    "<-",
    "..",
    "..<",
    "..+",
    "..-",
    "&",
    "|",
    ".>",
    "@",
    "+=",
    "++=",
    "-=",
    "--=",
    "*=",
    "/=",
    "^=",
    "++",
    "--",
    "...",
    "::"
  )
}

class YParser extends StandardTokenParsers with PackratParsers {

  import Interpolation._

  override val lexical = new YolaLexical

  def parse[T](grammar: PackratParser[T], r: Reader[Char]) = phrase(grammar)(lexical.read(r))

  def parseFromSource[T](src: io.Source, grammar: PackratParser[T]) =
    parseFromString(src.mkString, grammar)

  def parseFromString[T](src: String, grammar: PackratParser[T]) = {
    parse(grammar, new CharSequenceReader(src)) match {
      case Success(tree, _)       => tree
      case NoSuccess(error, rest) => problem(rest.pos, error)
    }
  }

  import lexical.{Dedent, Indent, Newline, RegexLit}

  lazy val regexLit: Parser[String] =
    elem("regex literal", _.isInstanceOf[RegexLit]) ^^ (_.chars)

  lazy val pos = positioned(success(new Positional {})) ^^ { _.pos }

  lazy val nl = rep1(Newline)

  lazy val onl = rep(Newline)

  lazy val number: PackratParser[Number] =
    numericLit ^^
      (n =>
        if (n startsWith "0x") {
          val num = BigInt(n substring 2, 16)

          if (num.isValidInt)
            num.intValue.asInstanceOf[Number]
          else
            num
        } else if (n matches ".*[.eE].*")
          n.toDouble.asInstanceOf[Number]
        else {
          val bi = BigInt(n)

          if (bi.isValidInt)
            bi.intValue.asInstanceOf[Number]
          else
            bi
        })

  lazy val source: PackratParser[BlockExpressionAST] =
    Newline ^^^ BlockExpressionAST(Nil) |
      statements ^^ BlockExpressionAST

  lazy val statements = rep1(statement)

  lazy val statement: PackratParser[StatementAST] =
    expressionStatement |
      declarationStatement

  lazy val expressionStatement: PackratParser[ExpressionAST] = expression <~ Newline

  lazy val declarationStatement: PackratParser[DeclarationStatementAST] = declaration <~ Newline

  def declarationdef: PackratParser[DeclarationStatementAST] =
    //		imports |
    //		natives |
    constants |
      variables |
      datatypes |
      definitions

  lazy val declaration: PackratParser[DeclarationStatementAST] = declarationdef

  lazy val constants =
    "val" ~> rep1sep(constant, ",") ^^ DeclarationBlockAST |
      "val" ~> Indent ~> rep1(constant <~ Newline) <~ Dedent ^^ DeclarationBlockAST

  lazy val constant =
    (pattern <~ "=") ~ pos ~ noAssignmentExpressionOrBlock ^^ {
      case struc ~ p ~ exp => ValAST(struc, p, exp)
    }

  lazy val variables: PackratParser[DeclarationStatementAST] =
    "var" ~> rep1sep(variable, ",") ^^ DeclarationBlockAST |
      "var" ~> Indent ~> rep1(variable <~ Newline) <~ Dedent ^^ DeclarationBlockAST

  lazy val variable =
    pos ~ ident ~ opt("=" ~> pos ~ noAssignmentExpressionOrBlock) ^^ {
      case p ~ n ~ Some(pe ~ e) => VarAST(p, n, Some((pe, e)))
    }

  lazy val datatypes =
    "data" ~> datatype |
      "data" ~> Indent ~> rep1(datatype <~ Newline) <~ Dedent ^^ DeclarationBlockAST

  lazy val datatype =
    pos ~ (ident <~ "=") ~ rep1sep(constructor, "|") ^^ {
      case p ~ typename ~ constructors => DataAST(p, typename, constructors)
    } |
      pos ~ constructor ^^ { case p ~ (c @ (n, _)) => DataAST(p, n, List(c)) }

  lazy val constructor: PackratParser[(String, List[Symbol])] =
    (ident <~ "(") ~ (rep1sep(ident, ",") <~ ")") ^^ {
      case name ~ fields => (name, fields map (Symbol(_)))
    } |
      ident ^^ ((_, Nil))

  lazy val definitions =
    "def" ~> definition |
      "def" ~> Indent ~> rep1(definition <~ Newline) <~ Dedent ^^ DeclarationBlockAST

  lazy val definition =
    pos ~ ident ~ opt("(" ~> rep1sep(pattern, ",") ~ opt("...") <~ ")") ~ (optionallyGuardedPart | guardedParts) ^^ {
      case p ~ n ~ None ~ ((gs, w)) =>
        DefAST(p, n, FunctionExpressionAST(p, n, Nil, false, gs, WhereClauseAST(w)))
      case p ~ n ~ Some(parms ~ a) ~ ((gs, w)) =>
        DefAST(p, n, FunctionExpressionAST(p, n, parms, a isDefined, gs, WhereClauseAST(w)))
    }

  lazy val optionallyGuardedPart
    : PackratParser[(List[FunctionPart], List[DeclarationStatementAST])] =
    opt("|" ~> guardExpression) ~ ("=" ~> expressionOrBlock | blockExpression) ~ opt(
      whereClause | Indent ~> whereClause <~ Newline <~ Dedent) ^^ {
      case g ~ b ~ w => (List(FunctionPart(g, b)), w getOrElse Nil)
    }

  lazy val guardExpression: PackratParser[ExpressionAST] =
    guardExpression ~ ("or" ~> guardAndExpression) ^^ {
      case lhs ~ rhs => OrExpressionAST(lhs, rhs)
    } |
      guardAndExpression

  lazy val guardAndExpression: PackratParser[ExpressionAST] =
    guardAndExpression ~ ("and" ~> guardNotExpression) ^^ {
      case lhs ~ rhs => AndExpressionAST(lhs, rhs)
    } |
      guardNotExpression

  lazy val guardNotExpression: PackratParser[ExpressionAST] =
    "not" ~> guardNotExpression ^^ NotExpressionAST |
      functionExpression

  lazy val guardedParts: PackratParser[(List[FunctionPart], List[DeclarationStatementAST])] =
    Indent ~> rep1(guardedPart) ~ opt(whereClause <~ Newline) <~ Dedent ^^ {
      case g ~ w => (g, w getOrElse Nil)
    }

  lazy val guardedPart =
    "|" ~> ("otherwise" ^^^ None | guardExpression ^^ (Some(_))) ~ ("=" ~> expressionOrBlock) <~ Newline ^^ {
      case g ~ b => FunctionPart(g, b)
    }

  lazy val whereClause: PackratParser[List[DeclarationStatementAST]] =
    "where" ~> repN(1, whereDefinition) |
      "where" ~> Indent ~> rep1(whereDefinition <~ Newline) <~ Dedent

  lazy val whereDefinition: PackratParser[DeclarationStatementAST] =
    pos ~ ident ~ ("(" ~> (rep1sep(pattern, ",") ~ opt("...")) <~ ")") ~ (optionallyGuardedPart | guardedParts) ^^ {
      case p ~ n ~ (parms ~ a) ~ ((gs, w)) =>
        DefAST(p, n, FunctionExpressionAST(p, n, parms, a isDefined, gs, WhereClauseAST(w)))
    } |
      constant

  lazy val expressionOrBlock = expression | blockExpression

  lazy val noAssignmentExpressionOrBlock = compoundExpression1 | blockExpression

  lazy val blockExpression = Indent ~> statements <~ Dedent ^^ BlockExpressionAST

  lazy val expression: PackratParser[ExpressionAST] = compoundExpression

  lazy val compoundExpressionStatement
    : PackratParser[StatementAST] = logicalExpression | declaration

  lazy val compoundExpression1: PackratParser[ExpressionAST] =
    ("(" ~> compoundExpressionStatement <~ ";") ~ (rep1sep(compoundExpressionStatement, ";") <~ ")") ^^ {
      case f ~ l => BlockExpressionAST(f :: l)
    } |
      orExpression1

  lazy val orExpression1: PackratParser[ExpressionAST] =
    orExpression1 ~ ("or" ~> andExpression1) ^^ { case lhs ~ rhs => OrExpressionAST(lhs, rhs) } |
      andExpression1

  lazy val andExpression1: PackratParser[ExpressionAST] =
    andExpression1 ~ ("and" ~> notExpression1) ^^ { case lhs ~ rhs => AndExpressionAST(lhs, rhs) } |
      notExpression1

  lazy val notExpression1: PackratParser[ExpressionAST] =
    "not" ~> notExpression1 ^^ NotExpressionAST |
      constructExpression

  lazy val compoundExpression: PackratParser[ExpressionAST] =
    ("(" ~> compoundExpressionStatement <~ ";") ~ (rep1sep(compoundExpressionStatement, ";") <~ ")") ^^ {
      case f ~ l => BlockExpressionAST(f :: l)
    } |
      logicalExpression

  lazy val logicalExpression = orExpression

  lazy val orExpression: PackratParser[ExpressionAST] =
    orExpression ~ ("or" ~> andExpression) ^^ { case lhs ~ rhs => OrExpressionAST(lhs, rhs) } |
      andExpression

  lazy val andExpression: PackratParser[ExpressionAST] =
    andExpression ~ ("and" ~> notExpression) ^^ { case lhs ~ rhs => AndExpressionAST(lhs, rhs) } |
      notExpression

  lazy val notExpression: PackratParser[ExpressionAST] =
    "not" ~> notExpression ^^ NotExpressionAST |
      assignmentExpression

  lazy val assignmentExpression: PackratParser[ExpressionAST] =
    rep1sep(pos ~ lvalueExpression, ",") ~ assignment ~ (rep1sep(pos ~ assignmentExpression, ",") | pos ~ blockExpression ^^ (List(
      _))) ^^ {
      case lhs ~ op ~ rhs =>
        AssignmentExpressionAST(lhs map { case p ~ e => (p, e) }, op, rhs map {
          case p ~ e                                 => (p, e)
        })
    } |
      constructExpression

  lazy val constructExpression
    : PackratParser[ExpressionAST] = "if" ~> expression ~ ("then" ~> expressionOrBlock | blockExpression) ~ rep(
    elif) ~ elsePart ^^ {
    case c ~ t ~ ei ~ e => ConditionalExpressionAST((c, t) +: ei, e)
  } |
    (("for" ~ Indent) ~> generators <~ nl) ~ ("yield" ~> expressionOrBlock <~ (Newline ~ Dedent)) ^^ {
      case g ~ b => ForYieldExpressionAST(g, b)
    } |
    ("for" ~> generators) ~ ((Indent ~ "yield") ~> expressionOrBlock) <~ (Newline ~ Dedent) ^^ {
      case g ~ b => ForYieldExpressionAST(g, b)
    } |
    ("for" ~> generators) ~ ("yield" ~> expressionOrBlock) ^^ {
      case g ~ b => ForYieldExpressionAST(g, b)
    } |
    opt(ident <~ ":") ~ ("for" ~> generators) ~ ("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^ {
      case l ~ g ~ b ~ e => ForExpressionAST(l, g, b, e)
    } |
    opt(ident <~ ":") ~ (("for" ~ Indent) ~> generators <~ (opt("do") ~ nl)) ~ ((statements ^^ BlockExpressionAST) <~ Dedent) ~ elsePart ^^ {
      case l ~ g ~ b ~ e => ForExpressionAST(l, g, b, e)
    } |
    opt(ident <~ ":") ~ ("while" ~> expression) ~ opt("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^ {
      case l ~ c ~ b ~ e => WhileExpressionAST(l, c, b, e)
    } |
    opt(ident <~ ":") ~ ("repeat" ~> expressionOrBlock) ^^ {
      case l ~ b => RepeatExpressionAST(l, b)
    } |
    functionExpression

  lazy val elsePart: PackratParser[Option[ExpressionAST]] = opt(onl ~> "else" ~> expressionOrBlock)

  lazy val elif = onl ~> "elif" ~> expression ~ ("then" ~> expressionOrBlock | blockExpression) ^^ {
    case c ~ t => (c, t)
  }

  lazy val generator = (pattern <~ "<-") ~ pos ~ expression ~ opt((onl ~ "if") ~> logicalExpression) ^^ {
    case s ~ p ~ t ~ f => GeneratorExpressionAST(s, p, t, f)
  }

  lazy val generators = rep1sep(generator, ";" | nl)

  lazy val lvalueExpression = applyExpression //generateDefinedExpression

  lazy val assignment = "=" | "+=" | "++=" | "-=" | "--=" | "*=" | "/=" | "//=" | "\\=" | "^=" | "<:=" | ">:="

  lazy val functionExpression: PackratParser[ExpressionAST] =
    lambdaExpression |
      partialFunctionExpression |
      comparisonExpression

  lazy val partialFunctionExpression: PackratParser[PartialFunctionExpressionAST] =
    Indent ~> rep1(lambdaExpression <~ Newline) <~ Dedent ^^ PartialFunctionExpressionAST

  lazy val parameters: PackratParser[(List[PatternAST], Boolean)] =
    "(" ~ ")" ^^^ (Nil, false) |
      "(" ~> rep1sep(pattern, ",") ~ opt("...") <~ ")" ^^ { case p ~ a => (p, a isDefined) } |
      repN(1, pattern) ~ opt("...") ^^ { case p ~ a                    => (p, a isDefined) }

  lazy val lambdaExpression: PackratParser[FunctionExpressionAST] =
    pos ~ parameters ~ opt("|" ~> guardExpression) ~ ("->" ~> opt(expressionOrBlock)) ^^ {
      case p ~ ((parms, a)) ~ g ~ b =>
        FunctionExpressionAST(p,
                              "$" + p.toString,
                              parms,
                              a,
                              List(FunctionPart(g, b.getOrElse(LiteralExpressionAST(())))),
                              WhereClauseAST(Nil))
    }
  //		"otherwise" ~> "->" ~> opt(expressionOrBlock) ^^ {
  //			b => FunctionExpressionAST( List(VariablePatternAST(null, "_")), false, List(FunctionPartExpressionAST(None, b.getOrElse(LiteralExpressionAST(())))), WhereClauseAST(Nil) ) }

  lazy val comparisonExpression: PackratParser[ExpressionAST] =
    pos ~ alternationExpression ~ rep1(
      ("==" | "!=" | "<" | ">" | "<=" | ">=") ~ pos ~ alternationExpression ^^ {
        case op ~ p ~ r => (op, p, r)
      }) ^^ {
      case pl ~ l ~ cs => ComparisonExpressionAST(pl, l, cs)
    } |
      pos ~ alternationExpression ~ ("in" | "not" ~ "in" ^^^ "notin") ~ pos ~ alternationExpression ^^ {
        case pl ~ l ~ op ~ pr ~ r => BinaryExpressionAST(pl, l, op, pr, r)
      } |
//      alternationExpression ~ "is" ~ ident ^^ { case e ~ _ ~ t => TypeExpressionAST( e, t ) } |
//      alternationExpression ~ ("is" ~> "not" ~> ident) ^^ { case e ~ t => NotExpressionAST( TypeExpressionAST(e, t) ) } |
      alternationExpression

  lazy val alternationExpression: PackratParser[ExpressionAST] =
    alternationExpression ~ ("|" ~> concatenationExpression) ^^ {
      case lhs ~ rhs => OrExpressionAST(lhs, rhs)
    } |
      concatenationExpression

  lazy val concatenationExpression: PackratParser[ExpressionAST] =
    concatenationExpression ~ ("&" ~> controlExpression) ^^ {
      case lhs ~ rhs => AndExpressionAST(lhs, rhs)
    } |
      controlExpression

  lazy val controlExpression: PackratParser[ExpressionAST] =
    "break" ~> pos ~ opt(ident) ~ opt("(" ~> additiveExpression /*consExpression*/ <~ ")") ^^ {
      case p ~ l ~ e => BreakExpressionAST(p, l, e)
    } |
      "continue" ~> pos ~ opt(ident) ^^ { case p ~ l => ContinueExpressionAST(p, l) } |
      "return" ~> opt(expression) ^^ (e =>
        ReturnExpressionAST(e.getOrElse(LiteralExpressionAST(())))) |
//      ("yield" ~> consExpression) ~ opt("do" ~> consExpression) ^^ { case e ~ r => YieldExpressionAST( e, r ) } |
      consExpression

  lazy val consExpression: PackratParser[ExpressionAST] =
    pos ~ rangeExpression ~ ("::" ~> pos) ~ consExpression ^^ {
      case ph ~ h ~ pt ~ t => ConsExpressionAST(ph, h, pt, t)
    } |
      rangeExpression

  lazy val rangeExpression: PackratParser[ExpressionAST] =
    pos ~ (additiveExpression <~ "..") ~ pos ~ additiveExpression ~ opt(
      "by" ~> pos ~ additiveExpression) ^^ {
      case pf ~ f ~ pt ~ t ~ Some(pb ~ b) => RangeExpressionAST(pf, f, pt, t, pb, b, true)
      case pf ~ f ~ pt ~ t ~ None =>
        RangeExpressionAST(pf, f, pt, t, null, LiteralExpressionAST(1), true)
    } |
      pos ~ (additiveExpression <~ "..<") ~ pos ~ additiveExpression ~ opt(
        "by" ~> pos ~ additiveExpression) ^^ {
        case pf ~ f ~ pt ~ t ~ Some(pb ~ b) => RangeExpressionAST(pf, f, pt, t, pb, b, false)
        case pf ~ f ~ pt ~ t ~ None =>
          RangeExpressionAST(pf, f, pt, t, null, LiteralExpressionAST(1), false)
      } |
      pos ~ (additiveExpression <~ "..+") ~ pos ~ additiveExpression ~ opt(
        "by" ~> pos ~ additiveExpression) ^^ {
        case pf ~ f ~ pt ~ t ~ Some(pb ~ b) =>
          RangeExpressionAST(pf, f, pt, BinaryExpressionAST(null, f, "+", null, t), pb, b, false)
        case pf ~ f ~ pt ~ t ~ None =>
          RangeExpressionAST(pf,
                             f,
                             pt,
                             BinaryExpressionAST(null, f, "+", null, t),
                             null,
                             LiteralExpressionAST(1),
                             false)
      } |
      pos ~ (additiveExpression <~ "..-") ~ pos ~ additiveExpression ~ opt(
        "by" ~> pos ~ additiveExpression) ^^ {
        case pf ~ f ~ pt ~ t ~ Some(pb ~ b) =>
          RangeExpressionAST(pf, f, pt, BinaryExpressionAST(null, f, "-", null, t), pb, b, false)
        case pf ~ f ~ pt ~ t ~ None =>
          RangeExpressionAST(pf,
                             f,
                             pt,
                             BinaryExpressionAST(null, f, "-", null, t),
                             null,
                             LiteralExpressionAST(-1),
                             false)
      } |
//      pos ~ (additiveExpression <~ "..") ~ opt("by" ~> pos ~ additiveExpression) ^^ {
//        case pf ~ f ~ None         => UnboundedLazyListExpressionAST(pf, f, null, LiteralExpressionAST(1))
//        case pf ~ f ~ Some(pb ~ b) => UnboundedLazyListExpressionAST(pf, f, pb, b)
//      } |
//      pos ~ additiveExpression ~ ("to" | "until") ~ pos ~ additiveExpression ~ opt(
//        "by" ~> pos ~ additiveExpression) ^^ {
//        case pf ~ f ~ op ~ pt ~ t ~ Some(pb ~ b) =>
//          SequenceExpressionAST(pf, f, pt, t, pb, b, if (op == "to") true else false)
//        case pf ~ f ~ op ~ pt ~ t ~ None =>
//          SequenceExpressionAST(pf,
//                                f,
//                                pt,
//                                t,
//                                null,
//                                LiteralExpressionAST(1),
//                                if (op == "to") true else false)
//      } |
      additiveExpression

  lazy val additiveExpression: PackratParser[ExpressionAST] =
    pos ~ additiveExpression ~ ("+" | "-") ~ pos ~ multiplicativeExpression ^^ {
      case pl ~ l ~ o ~ pr ~ r => BinaryExpressionAST(pl, l, o, pr, r)
    } |
      multiplicativeExpression

  lazy val multiplicativeExpression: PackratParser[ExpressionAST] =
    pos ~ multiplicativeExpression ~ ("*" | "/" | """\""" | "%" | "\\%" | "//" | "mod" | "div" | ">>>" | "<<") ~ pos ~ exponentialExpression ^^ {
      case pl ~ l ~ o ~ pr ~ r => BinaryExpressionAST(pl, l, o, pr, r)
    } |
      pos ~ multiplicativeExpression ~ pos ~ applyExpression ^^ {
        case pl ~ l ~ pr ~ r => BinaryExpressionAST(pl, l, "adj", pr, r)
      } |
      exponentialExpression

  lazy val exponentialExpression: PackratParser[ExpressionAST] =
    pos ~ unaryExpression ~ "^" ~ pos ~ exponentialExpression ^^ {
      case pl ~ l ~ _ ~ pr ~ r => BinaryExpressionAST(pl, l, "^", pr, r)
    } |
      unaryExpression

  lazy val unaryExpression: PackratParser[ExpressionAST] =
    "-" ~> pos ~ incrementExpression ^^ {
      case _ ~ LiteralExpressionAST(n: Number) => LiteralExpressionAST(n) // todo: negate
      case p ~ v                               => UnaryExpressionAST("-", p, v)
    } |
//      "." ~> incrementExpression ^^ DereferenceExpressionAST |
      incrementExpression

  lazy val incrementExpression: PackratParser[ExpressionAST] =
    ("++" | "--") ~ pos ~ applyExpression /*generateDefinedExpression*/ ^^ {
      case o ~ p ~ e => UnaryExpressionAST(o + "_", p, e)
    } |
      pos ~ applyExpression /*generateDefinedExpression*/ ~ ("++" | "--") ^^ {
        case p ~ e ~ o => UnaryExpressionAST("_" + o, p, e)
      } |
      applyExpression /*generateDefinedExpression*/
//
//  lazy val generateDefinedExpression: PackratParser[ExpressionAST] =
//    "!" ~> pos ~ applyExpression ^^ {
//      case p ~ c => GenerateExpressionAST( p, c ) } |
//  applyExpression

  lazy val applyExpression: PackratParser[ExpressionAST] =
    pos ~ applyExpression ~ pos ~ ("(" ~> repsep(pos ~ expression, ",") <~ ")") ^^ {
      case fp ~ f ~ ap ~ args =>
        ApplyExpressionAST(fp, f, ap, args map { case p ~ e => (p, e) }, false)
    } |
      pos ~ applyExpression ~ ("." ~> pos) ~ (ident | stringLit) ^^ {
        case fp ~ e ~ ap ~ f => DotExpressionAST(fp, e, ap, f)
      } |
      primaryExpression

  lazy val mapEntry = keyExpression ~ (":" ~> expression) ^^ {
    case VariableExpressionAST(_, k) ~ v => LiteralExpressionAST(k) -> v
    case k ~ v                           => (k, v)
  }

  lazy val keyExpression = additiveExpression

//  lazy val comprehension: PackratParser[ComprehensionAST] = (consExpression <~ "|") ~ generators ^^ { case e ~ g => ComprehensionAST( e, g ) }

  lazy val primaryExpression: PackratParser[ExpressionAST] =
//    pos ~ regexLit ^^ {
//      case p ~ r => RegexLiteralAST( p, r ) } |
    number ^^ LiteralExpressionAST |
      pos ~ stringLit ^^ {
        case p ~ s =>
          if (s.length > 0 && s.charAt(0) >= INTERPOLATION_DELIMITER) {
            val buf = new ListBuffer[ExpressionAST]

            for (m <- INTERPOLATED_PATTERN.findAllMatchIn(s))
              m.matched.charAt(0) match {
                case INTERPOLATION_LITERAL => buf.append(LiteralExpressionAST(m.group(1)))
                case INTERPOLATION_VARIABLE =>
                  buf.append(VariableExpressionAST(p, m.group(1)))
                case INTERPOLATION_EXPRESSION =>
                  val parser = new YParser

                  buf += parser
                    .parseFromString(m.group(1), parser.expressionStatement)
                    .asInstanceOf[ExpressionAST]
              }

            InterpolationExpressionAST(buf.toList)
          } else
            LiteralExpressionAST(s)
      } |
      "(" ~> infix <~ ")" ^^ (op =>
        FunctionExpressionAST(
          null,
          "",
          List(VariablePatternAST(null, "a"), VariablePatternAST(null, "b")),
          false,
          List(
            FunctionPart(None,
                         BinaryExpressionAST(null,
                                             VariableExpressionAST(null, "a"),
                                             op,
                                             null,
                                             VariableExpressionAST(null, "b")))),
          WhereClauseAST(Nil)
        )) |
      ("(" ~> pos) ~ applyExpression ~ (infix <~ ")") ^^ {
        case p ~ e ~ o =>
          FunctionExpressionAST(
            p,
            o,
            List(VariablePatternAST(p, "a")),
            false,
            List(
              FunctionPart(None,
                           BinaryExpressionAST(null, e, o, p, VariableExpressionAST(null, "a")))),
            WhereClauseAST(Nil)
          )
      } |
      "(" ~> infix ~ pos ~ applyExpression <~ ")" ^^ {
        case o ~ p ~ e =>
          FunctionExpressionAST(
            p,
            o,
            List(VariablePatternAST(p, "a")),
            false,
            List(
              FunctionPart(None,
                           BinaryExpressionAST(null, VariableExpressionAST(null, "a"), o, p, e))),
            WhereClauseAST(Nil)
          )
      } |
      ("true" | "false") ^^ (b => LiteralExpressionAST(b.toBoolean)) |
      "(" ~ ")" ^^^ LiteralExpressionAST(()) |
      "null" ^^^ LiteralExpressionAST(null) |
      pos ~ ident ^^ { case p ~ n => VariableExpressionAST(p, n) } |
      "[" ~> repsep(expression, ",") <~ "]" ^^ { l =>
        ListExpressionAST(l)
      } |
//      "[" ~> comprehension <~ "]" ^^ ListComprehensionExpressionAST |
      ("(" ~> expression <~ ",") ~ (rep1sep(expression, ",") <~ ")") ^^ {
        case e ~ l => TupleExpressionAST(e +: l)
      } |
      "{" ~> repsep(mapEntry, ",") <~ "}" ^^ MapExpressionAST |
      "(" ~> expression <~ ")"

  lazy val infix =
    "+" | "-" | "*" | "/" | """\""" | "\\%" | "^" | "%" |
      "mod" | "div" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in" ^^^ "notin" |
      "::" //todo: add support for ranges

  lazy val pattern: PackratParser[PatternAST] = altPattern

  lazy val altPattern: PackratParser[PatternAST] =
    pos ~ (altPattern <~ "|") ~ rep1sep(altPattern, "|") ^^ {
      case p ~ e ~ l => AlternationPatternAST(p, e +: l)
    } |
      namedPattern

  lazy val namedPattern: PackratParser[PatternAST] =
    pos ~ (ident <~ "@") ~ typePattern ^^ { case p ~ name ~ pat => NamedPatternAST(p, name, pat) } |
      typePattern

  lazy val typePattern: PackratParser[PatternAST] =
    consPattern ~ (":" ~> ident) ^^ { case pat ~ typename => TypePatternAST(pat, typename) } |
      consPattern

  lazy val consPattern: PackratParser[PatternAST] =
    pos ~ primaryPattern ~ ("::" ~> consPattern) ^^ { case p ~ h ~ t => ConsPatternAST(p, h, t) } |
      primaryPattern

  lazy val primaryPattern: PackratParser[PatternAST] =
    pos ~ number ^^ { case p ~ l          => LiteralPatternAST(p, l) } |
      pos ~ stringLit ^^ { case p ~ l     => LiteralPatternAST(p, l) } |
      pos ~ "(" ~ ")" ^^ { case p ~ _ ~ _ => LiteralPatternAST(p, ()) } |
      pos ~ "null" ^^ { case p ~ _        => LiteralPatternAST(p, null) } |
//      pos ~ ident ~ ("(" ~> rep1sep(structure, ",") <~ ")") ^^ { case p ~ n ~ l => RecordPatternAST( p, n, l.toVector ) } |
      pos ~ ident ^^ { case p ~ n => VariablePatternAST(p, n) } |
      pos ~ ("(" ~> pattern <~ ",") ~ (rep1sep(pattern, ",") <~ ")") ^^ {
        case p ~ e ~ l => TuplePatternAST(p, e +: l)
      } |
      pos ~ ("[" ~> repsep(pattern, ",") <~ "]") ^^ { case p ~ l    => ListPatternAST(p, l) } |
      pos ~ "{" ~ repsep(ident, ",") ~ "}" ^^ { case p ~ _ ~ es ~ _ => MapPatternAST(p, es.toSet) } |
      "(" ~> pattern <~ ")"

}

//todo: cset operations + (including adding string to cset), -, *, - (complement)
//todo: bitwise operations: &&, ||, ^^, ~, <<, >>, >>>
//todo: mutable record fields (!r on lhs should generate l-values of record fields)
//todo: set operations +, -, *, - (complement)
//todo: add +.. and -.. for ranges
//todo: Nim style named arguments
//todo: var a, b, c
//todo: allow indentation after operators (as in Nim)
