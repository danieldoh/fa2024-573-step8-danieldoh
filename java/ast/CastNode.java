package ast;

import ast.visitor.ASTVisitor;
import compiler.Scope;

public class CastNode extends ExpressionNode{
    @Override
    public <R> R accept(ASTVisitor<R> visitor) {
        return visitor.visit(this);
    }
    private ExpressionNode expr;
    public CastNode(Scope.Type type, ExpressionNode expr){
        this.type = type;
        this.expr = expr;
    }

    public TypedASTNode getExpr(){
        return expr;
    }
}
