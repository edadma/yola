package xyz.hyperreal.yola

class Interpreter {

  def interp() {}

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
