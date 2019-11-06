package xyz.hyperreal.yola

import util.parsing.input.Position


trait AST

case class SourceAST( statements: List[StatementAST] ) extends AST

trait StatementAST extends AST

trait DeclarationStatementAST extends StatementAST
//case class ImportAST( qual: String, names: List[(String, Option[String])] ) extends DeclarationStatementAST
//case class NativeAST( pkg: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
//case class FunctionAST( cls: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
case class ValAST( struc: StructureAST, pos: Position, exp: ExpressionAST ) extends DeclarationStatementAST
case class VarAST( pos: Position, var name: String, oname: String, init: Option[ExpressionAST] ) extends DeclarationStatementAST
case class DataAST( pos: Position, name: String, constructors: List[(String, List[Symbol])] ) extends DeclarationStatementAST
case class DefAST( oname: String, func: FunctionExpressionAST ) extends DeclarationStatementAST

case class DeclarationBlockAST( decls: List[DeclarationStatementAST] ) extends DeclarationStatementAST

trait ExpressionAST extends StatementAST
case class FunctionExpressionAST( var name: String, pos: Position, parms: List[StructureAST], arb: Boolean, parts: List[FunctionPartExpressionAST], where: WhereClauseAST ) extends ExpressionAST
case class PartialFunctionExpressionAST( cases: List[FunctionExpressionAST] ) extends ExpressionAST
case class ApplyExpressionAST( epos: Position, f: ExpressionAST, apos: Position, args: List[(Position, ExpressionAST)], var tailrecursive: Boolean ) extends ExpressionAST
case class DotExpressionAST( epos: Position, expr: ExpressionAST, apos: Position, field: Symbol ) extends ExpressionAST
case class LiteralExpressionAST( v: Any ) extends ExpressionAST
case class VariableExpressionAST( pos: Position, var name: String, oname: String ) extends ExpressionAST
case class BinaryExpressionAST( lpos: Position, left: ExpressionAST, op: Symbol, rpos: Position, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpressionAST( op: Symbol, pos: Position, expr: ExpressionAST ) extends ExpressionAST
case class AssignmentExpressionAST( lhs: List[(Position, ExpressionAST)], op: Symbol, rhs: List[(Position, ExpressionAST)] ) extends ExpressionAST
case class BlockExpressionAST( l: List[StatementAST] ) extends ExpressionAST
case class ListExpressionAST( l: List[ExpressionAST] ) extends ExpressionAST
case class OrExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class AndExpressionAST( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class NotExpressionAST( expr: ExpressionAST ) extends ExpressionAST
case class ConditionalExpressionAST( cond: Seq[(ExpressionAST, ExpressionAST)], els: Option[ExpressionAST] ) extends ExpressionAST
case class WhileExpressionAST( label: Option[String], cond: ExpressionAST, body: Option[ExpressionAST], els: Option[ExpressionAST] ) extends ExpressionAST
case class BreakExpressionAST( pos: Position, label: Option[String], expr: Option[ExpressionAST] ) extends ExpressionAST
case class ContinueExpressionAST( pos: Position, label: Option[String] ) extends ExpressionAST
case class ReturnExpressionAST( expr: ExpressionAST ) extends ExpressionAST

case class FunctionPartExpressionAST( guard: Option[ExpressionAST], body: ExpressionAST )

case class WhereClauseAST( where: List[DeclarationStatementAST] ) extends AST

trait StructureAST extends AST
case class NamedStructureAST( pos: Position, var alias: String, s: StructureAST ) extends StructureAST
case class TypeStructureAST( s: StructureAST, typename: String ) extends StructureAST
