package ast.visitor;

import java.util.LinkedList;
import java.util.List;

import assembly.CodeObject;
import ast.ASTNode;
import ast.AddrOfNode;
import ast.AssignNode;
import ast.BinaryOpNode;
import ast.CallNode;
import ast.IfStatementNode;
import ast.WhileNode;
import ast.IntLitNode;
import ast.MallocNode;
import ast.PtrDerefNode;
import ast.ReadNode;
import ast.ReturnNode;
import ast.StatementListNode;
import ast.StatementNode;
import ast.UnaryOpNode;
import ast.VarNode;
import ast.WriteNode;
import ast.CondNode;
import ast.ExpressionNode;
import ast.FunctionListNode;
import ast.FunctionNode;
import ast.FloatLitNode;
import ast.FreeNode;
import ast.CastNode;

/**
 * Abstract visitor class that implements the visitor pattern over the AST
 * 
 * Note that this is a somewhat non-standard visitor implementation because 
 * the traversal logic is in this abstract class, rather than in the AST nodes
 * themselves, where it would normally be. (This is so that subclasses can more
 * easily change the traversal order if need be.)
 * 
 * The way to use this class is to extend it to build your custom visitor logic.
 * See {@link PrintVisitor} for an example of a fully-implemented visitor.
 * 
 * The <code>preprocess</code> for each AST node type runs before the children of
 * a node are visited. The <code>postprocess</code> for each AST node type runs after
 * the children are visited. For post-order algorithms, then, only <code>postprocess</code>
 * methods for each node type need to be changed (similarly, pre-order algorithms only
 * need to change <code>preprocess</code>). More complicated visitors may need to
 * override both (e.g., to generate labels before visiting children).
 * 
 * @param <R> The data type returned by the visitor. For code generation, this is {@link CodeObject}
 */
public abstract class AbstractASTVisitor<R> implements ASTVisitor<R> {

	protected AbstractASTVisitor() { }

	@Override
	public R run(ASTNode node) {
		return node.accept(this);
	}

	@Override
	public R visit(VarNode node) {
		preprocess(node);
		return postprocess(node);
	}

	@Override
	public R visit(IntLitNode node) {
		preprocess(node);
		return postprocess(node);
	}

	@Override
	public R visit(FloatLitNode node) {
		preprocess(node);
		return postprocess(node);
	}

	@Override
	public R visit(BinaryOpNode node) {
		preprocess(node);
		R left = node.getLeft().accept(this);
		R right = node.getRight().accept(this);
		return postprocess(node, left, right);
	}

	@Override
	public R visit(UnaryOpNode node) {
		preprocess(node);
		R child = node.getExpr().accept(this);
		return postprocess(node, child);
	}

	@Override
	public R visit(AssignNode node) {
		preprocess(node);
		R left = node.getLeft().accept(this);
		R right = node.getRight().accept(this);
		return postprocess(node, left, right);
	}

	@Override
	public R visit(StatementListNode node) {
		preprocess(node);
		List<R> rs = new LinkedList<R>();
		for (StatementNode n : node.getStatements()) {
			rs.add(n.accept(this));	
		}
		R retval = postprocess(node, rs);
		return retval;
	}

	@Override
	public R visit(ReadNode node) {
		preprocess(node);
		R var = node.getVarNode().accept(this);
		return postprocess(node, var);
	}

	@Override
	public R visit(WriteNode node) {
		preprocess(node);
		R writeExpr = node.getWriteExpr().accept(this);
		return postprocess(node, writeExpr);
	}

	@Override
	public R visit(CondNode node) {
		preprocess(node);
		R left = node.getLeft().accept(this);
		R right = node.getRight().accept(this);
		return postprocess(node, left, right);
	}

	@Override
	public R visit(IfStatementNode node) {
		preprocess(node);
		R cond = node.getCondExpr().accept(this);
		R tlist = node.getThenBlock().accept(this);
		R elist = null;
		if (node.getElseBlock() != null) { //only traverse the else block if it exists
			elist = node.getElseBlock().accept(this);
		}
		return postprocess(node, cond, tlist, elist);
	}

	@Override
	public R visit(WhileNode node) {
		preprocess(node);
		R cond = node.getCond().accept(this);
		R slist = node.getSlist().accept(this);
		return postprocess(node, cond, slist);
	}

	@Override
	public R visit(ReturnNode node) {
		preprocess(node);
		R retExpr = null;
		if (node.getRetExpr() != null) {
			retExpr = node.getRetExpr().accept(this);
		}
		return postprocess(node, retExpr);
	}

	@Override
	public R visit(FunctionNode node) {
		preprocess(node);
		R body = node.getFuncBody().accept(this);
		return postprocess(node, body);
	}

	@Override
	public R visit(FunctionListNode node) {
		preprocess(node);
		List<R> fs = new LinkedList<R>();
		for (StatementNode n : node.getFunctions()) {
			fs.add(n.accept(this));	
		}
		R retval = postprocess(node, fs);
		return retval;
	}

	@Override
	public R visit(CallNode node) {
		preprocess(node);
		List<R> as = new LinkedList<R>();
		for (ExpressionNode n : node.getArgs()) {
			as.add(n.accept(this));
		}
		R retval = postprocess(node, as);
		return retval;
	}

	@Override
	public R visit(PtrDerefNode node) {
		preprocess(node);
		R expr = node.getExpr().accept(this);
		R retval = postprocess(node, expr);
		return retval;
	}

	@Override
	public R visit(AddrOfNode node) {
		preprocess(node);
		R expr = node.getExpr().accept(this);
		R retval = postprocess(node, expr);
		return retval;
	}

	@Override
	public R visit(MallocNode node) {
		preprocess(node);
		R arg = node.getArg().accept(this);
		R retval = postprocess(node, arg);
		return retval;
	}

	@Override
	public R visit(FreeNode node) {
		preprocess(node);
		R arg = node.getArg().accept(this);
		R retval = postprocess(node, arg);
		return retval;
	}

	@Override
	public R visit(CastNode node) {
		preprocess(node);
		R arg = node.getExpr().accept(this);
		return postprocess(node, arg);
	}

	protected R postprocess(VarNode node) {
		return null;
	}

	protected R postprocess(IntLitNode node) {
		return null;
	}

	protected R postprocess(FloatLitNode node) {
		return null;
	}

	protected R postprocess(BinaryOpNode node, R left, R right) {
		return null;
	}

	protected R postprocess(UnaryOpNode node, R expr) {
		return null;
	}

	protected R postprocess(AssignNode node, R left, R right) {
		return null;
	}

	protected R postprocess(StatementListNode node, List<R> statements) {
		return null;
	}

	protected R postprocess(ReadNode node, R var) {
		return null;
	}

	protected R postprocess(WriteNode node, R writeExpr) {
		return null;
	}

	protected R postprocess(CondNode node, R left, R right) {
		return null;
	}

	protected R postprocess(IfStatementNode node, R cond, R tlist, R elist) {
		return null;
	}

	protected R postprocess(WhileNode node, R cond, R slist) {
		return null;
	}

	protected R postprocess(ReturnNode node, R retExpr) {
		return null;
	}

	protected R postprocess(FunctionNode node, R body) {
		return null;
	}

	protected R postprocess(FunctionListNode node, List<R> functions) {
		return null;
	}

	protected R postprocess(CallNode node, List<R> args) {
		return null;
	}

	protected R postprocess(PtrDerefNode node, R expr) {
		return null;
	}

	protected R postprocess(AddrOfNode node, R expr) {
		return null;
	}

	protected R postprocess(MallocNode node, R arg) {
		return null;
	}

	protected R postprocess(FreeNode node, R arg) {
		return null;
	}

	protected R postprocess(CastNode node, R arg){
		return null;
	}

	protected void preprocess(VarNode node) {
		return;
	}

	protected void preprocess(IntLitNode node) {
		return;
	}

	protected void preprocess(FloatLitNode node) {
		return;
	}

	protected void preprocess(BinaryOpNode node) {
		return;
	}

	protected void preprocess(UnaryOpNode node) {
		return;
	}

	protected void preprocess(AssignNode node) {
		return;
	}

	protected void preprocess(StatementListNode node) {
		return;
	}

	protected void preprocess(ReadNode node) {
		return;
	}

	protected void preprocess(WriteNode node) {
		return;
	}

	protected void preprocess(CondNode node) {
		return;
	}

	protected void preprocess(IfStatementNode node) {
		return;
	}

	protected void preprocess(WhileNode node) {
		return;
	}

	protected void preprocess(ReturnNode node) {
		return;
	}

	protected void preprocess(FunctionNode node) {
		return;
	}

	protected void preprocess(FunctionListNode node) {
		return;
	}

	protected void preprocess(CallNode node) {
		return;
	}

	protected void preprocess(PtrDerefNode node) {
		return;
	}

	protected void preprocess(AddrOfNode node) {
		return;
	}
	
	protected void preprocess(MallocNode node) {
		return;
	}

	protected void preprocess(FreeNode node) {
		return;
	}

	protected void preprocess(CastNode node) {
		return;
	}

}
