package xyz.hyperreal.yola

import util.parsing.input.Position

trait AST

case class SourceAST(statements: List[StatementAST]) extends AST

trait StatementAST extends AST

trait DeclarationStatementAST extends StatementAST
//case class ImportAST( qual: String, names: List[(String, Option[String])] ) extends DeclarationStatementAST
//case class NativeAST( pkg: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
//case class FunctionAST( cls: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
case class ValAST(pat: PatternAST, pos: Position, expr: ExpressionAST)
    extends DeclarationStatementAST
case class VarAST(pos: Position, var name: String, init: Option[(Position, ExpressionAST)])
    extends DeclarationStatementAST
case class DataAST(pos: Position, name: String, constructors: List[(String, List[Symbol])])
    extends DeclarationStatementAST
case class DefAST(pos: Position, name: String, func: FunctionExpressionAST)
    extends DeclarationStatementAST

case class DeclarationBlockAST(decls: List[DeclarationStatementAST]) extends DeclarationStatementAST

trait ExpressionAST                         extends StatementAST
case class SectionExpressionAST(op: String) extends ExpressionAST
case class LeftSectionExpressionAST(pos: Position,
                                    expr: ExpressionAST,
                                    lambda: FunctionExpressionAST,
                                    op: String,
                                    var closure: Option[Boolean] = None)
    extends ExpressionAST
case class RightSectionExpressionAST(op: String,
                                     pos: Position,
                                     expr: ExpressionAST,
                                     lambda: FunctionExpressionAST,
                                     var closure: Option[Boolean] = None)
    extends ExpressionAST
case class ListComprehensionExpressionAST(comprehension: ComprehensionAST)  extends ExpressionAST
case class PartialFunctionExpressionAST(cases: List[FunctionExpressionAST]) extends ExpressionAST
case class ApplyExpressionAST(fpos: Position,
                              f: ExpressionAST,
                              apos: Position,
                              args: List[(Position, ExpressionAST)],
                              var tailrecursive: Boolean)
    extends ExpressionAST
case class DotExpressionAST(epos: Position, expr: ExpressionAST, apos: Position, field: String)
    extends ExpressionAST
case class LiteralExpressionAST(v: Any)                           extends ExpressionAST
case class VariableExpressionAST(pos: Position, var name: String) extends ExpressionAST
case class ComparisonExpressionAST(pos: Position,
                                   expr: ExpressionAST,
                                   comparisons: List[(String, Position, ExpressionAST)])
    extends ExpressionAST
case class BinaryExpressionAST(lpos: Position,
                               left: ExpressionAST,
                               op: String,
                               rpos: Position,
                               right: ExpressionAST)
    extends ExpressionAST
case class RangeExpressionAST(fpos: Position,
                              from: ExpressionAST,
                              tpos: Position,
                              to: ExpressionAST,
                              bpos: Position,
                              by: ExpressionAST,
                              incl: Boolean)
    extends ExpressionAST
case class ConsExpressionAST(lpos: Position,
                             left: ExpressionAST,
                             rpos: Position,
                             right: ExpressionAST)
    extends ExpressionAST
case class UnaryExpressionAST(op: String, pos: Position, expr: ExpressionAST) extends ExpressionAST
case class AssignmentExpressionAST(lhs: List[(Position, ExpressionAST)],
                                   op: String,
                                   rhs: List[(Position, ExpressionAST)])
    extends ExpressionAST
case class BlockExpressionAST(l: List[StatementAST])      extends ExpressionAST
case class ListExpressionAST(l: List[ExpressionAST])      extends ExpressionAST
case class TupleExpressionAST(elems: List[ExpressionAST]) extends ExpressionAST
case class MapExpressionAST(MapExpressionAST: List[(ExpressionAST, ExpressionAST)])
    extends ExpressionAST
case class OrExpressionAST(left: ExpressionAST, right: ExpressionAST)  extends ExpressionAST
case class AndExpressionAST(left: ExpressionAST, right: ExpressionAST) extends ExpressionAST
case class NotExpressionAST(expr: ExpressionAST)                       extends ExpressionAST
case class ConditionalExpressionAST(cond: Seq[(ExpressionAST, ExpressionAST)],
                                    els: Option[ExpressionAST])
    extends ExpressionAST
case class WhileExpressionAST(label: Option[String],
                              cond: ExpressionAST,
                              body: Option[ExpressionAST],
                              els: Option[ExpressionAST])
    extends ExpressionAST
case class RepeatExpressionAST(label: Option[String], body: ExpressionAST) extends ExpressionAST
case class ForLoopExpressionAST(label: Option[String],
                                gen: GeneratorExpressionAST,
                                body: Option[ExpressionAST],
                                els: Option[ExpressionAST])
    extends ExpressionAST
case class ForExpressionAST(label: Option[String],
                            gen: List[GeneratorExpressionAST],
                            body: ExpressionAST,
                            els: Option[ExpressionAST])
    extends ExpressionAST
case class GeneratorExpressionAST(pattern: PatternAST,
                                  pos: Position,
                                  iterable: ExpressionAST,
                                  filter: Option[ExpressionAST])
    extends ExpressionAST
case class BreakExpressionAST(pos: Position, label: Option[String], expr: Option[ExpressionAST])
    extends ExpressionAST
case class ContinueExpressionAST(pos: Position, label: Option[String]) extends ExpressionAST
case class ReturnExpressionAST(expr: ExpressionAST)                    extends ExpressionAST
case class InterpolationExpressionAST(l: List[ExpressionAST])          extends ExpressionAST

case class FunctionExpressionAST(pos: Position,
                                 name: String,
                                 parms: List[PatternAST],
                                 arb: Boolean,
                                 parts: List[FunctionPart],
                                 where: WhereClauseAST)
    extends ExpressionAST { var scope: Scope = null }

case class FunctionPart(guard: Option[ExpressionAST], body: ExpressionAST) extends AST

case class ComprehensionAST(expr: ExpressionAST, gen: List[GeneratorExpressionAST]) extends AST

case class WhereClauseAST(where: List[DeclarationStatementAST]) extends AST

trait PatternAST                                                              extends AST
case class NamedPatternAST(pos: Position, var alias: String, pat: PatternAST) extends PatternAST
case class VariablePatternAST(pos: Position, var name: String)                extends PatternAST
case class TypePatternAST(s: PatternAST, typename: String)                    extends PatternAST
case class TuplePatternAST(pos: Position, elems: List[PatternAST])            extends PatternAST
case class ListPatternAST(pos: Position, elems: List[PatternAST])             extends PatternAST
case class ConsPatternAST(pos: Position, head: PatternAST, tail: PatternAST)  extends PatternAST
case class MapPatternAST(pos: Position, entries: Set[String])                 extends PatternAST
case class LiteralPatternAST(pos: Position, lit: Any)                         extends PatternAST
case class AlternationPatternAST(pos: Position, alts: List[PatternAST])       extends PatternAST
