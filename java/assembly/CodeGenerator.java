package assembly;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import compiler.Scope.InnerType;
import compiler.Scope.SymbolTableEntry;
import ast.visitor.AbstractASTVisitor;

import ast.*;
import assembly.instructions.*;
import compiler.LocalScope;
import compiler.Scope;

public class CodeGenerator extends AbstractASTVisitor<CodeObject> {

	int intRegCount;
	int floatRegCount;
	static final public char intTempPrefix = 't';
	static final public char floatTempPrefix = 'f';
	
	int loopLabel;
	int elseLabel;
	int outLabel;

	String currFunc;
	
	public CodeGenerator() {
		loopLabel = 0;
		elseLabel = 0;
		outLabel = 0;
		intRegCount = 0;		
		floatRegCount = 0;
	}

	public int getIntRegCount() {
		return intRegCount;
	}

	public int getFloatRegCount() {
		return floatRegCount;
	}
	
	/**
	 * Generate code for Variables
	 * 
	 * Create a code object that just holds a variable
	 * 
	 * Important: add a pointer from the code object to the symbol table entry
	 *            so we know how to generate code for it later (we'll need to find
	 *            the address)
	 * 
	 * Mark the code object as holding a variable, and also as an lval
	 */
	@Override
	protected CodeObject postprocess(VarNode node) {
		
		Scope.SymbolTableEntry sym = node.getSymbol();
		
		CodeObject co = new CodeObject(sym);
		co.lval = true;
		co.type = node.getType();

		return co;
	}

	/** Generate code for IntLiterals
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(IntLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a register
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new Li(generateTemp(Scope.InnerType.INT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/** Generate code for FloatLiteras
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(FloatLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a regisster
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new FImm(generateTemp(Scope.InnerType.FLOAT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/**
	 * Generate code for binary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from left child
	 * Step 1a: if left child is an lval, add a load to get the data
	 * Step 2: add code from right child
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate binary operation using temps from left and right
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(BinaryOpNode node, CodeObject left, CodeObject right) {

		CodeObject co = new CodeObject();
		
		/* FILL IN FROM STEP 2 */
		if (left.isVar() || left.lval) {
			left = rvalify(left);
		}
		co.code.addAll(left.code);

		if (right.isVar() || right.lval) {
			right = rvalify(right);
		}
		co.code.addAll(right.code);
		
		InstructionList il = new InstructionList();

		//String dest = generateTemp(left.getType().type);
		String dest;

		if(left.getType().type == InnerType.INT && right.getType().type == InnerType.FLOAT) {
			left.type = new Scope.Type(InnerType.FLOAT);
			dest = generateTemp(InnerType.FLOAT);
			il.add(new IMovf(left.temp, dest));
			left.temp = dest;
		}
		else if(left.getType().type == InnerType.FLOAT && right.getType().type == InnerType.INT) {
			right.type = new Scope.Type(InnerType.FLOAT);
			dest = generateTemp(InnerType.FLOAT);
			il.add(new IMovf(right.temp, dest));
			right.temp = dest;
		}
		else {
			dest = generateTemp(left.getType().type);
		}

		switch (node.getOp()) {
			case ADD:

				if (left.getType().type == Scope.InnerType.PTR && right.getType().type == Scope.InnerType.INT) {
					Instruction addp = new Add(left.temp, right.temp, dest);
					il.add(addp);
				} else if (left.getType().type == Scope.InnerType.INT) {
					Instruction addi = new Add(left.temp, right.temp, dest);
					il.add(addi);
				} else if (left.getType().type == Scope.InnerType.FLOAT) {
					Instruction fadd = new FAdd(left.temp, right.temp, dest);
					il.add(fadd);
				} else {
					throw new Error("Invalid types for ADD");
				}
				break;

			case SUB:
				if (left.getType().type == Scope.InnerType.PTR && right.getType().type == Scope.InnerType.INT) {
					Instruction subp = new Sub(left.temp, right.temp, dest);
					il.add(subp);
				} else if (left.getType().type == Scope.InnerType.INT) {
					Instruction subi = new Sub(left.temp, right.temp, dest);
					il.add(subi);
				} else if (left.getType().type == Scope.InnerType.FLOAT) {
					Instruction subf = new FSub(left.temp, right.temp, dest);
					il.add(subf);
				} else {
					throw new Error("Invalid types for SUB");
				}
				break;

			case MUL:
				if (left.getType().type == Scope.InnerType.PTR && right.getType().type == Scope.InnerType.INT) {
					Instruction mulp = new Mul(left.temp, right.temp, dest);
					il.add(mulp);
				} else if (left.getType().type == Scope.InnerType.INT) {
					Instruction muli = new Mul(left.temp, right.temp, dest);
					il.add(muli);
				} else if (left.getType().type == Scope.InnerType.FLOAT) {
					Instruction mulf = new FMul(left.temp, right.temp, dest);
					il.add(mulf);
				} else {
					throw new Error("Invalid types for MUL");
				}
				break;
				
			case DIV:
				if (left.getType().type == Scope.InnerType.PTR && right.getType().type == Scope.InnerType.INT) {
					Instruction divp = new Div(left.temp, right.temp, dest);
					il.add(divp);
				} else if (left.getType().type == Scope.InnerType.INT) {
					Instruction divi = new Div(left.temp, right.temp, dest);
					il.add(divi);
				} else if (left.getType().type == Scope.InnerType.FLOAT) {
					Instruction divf = new FDiv(left.temp, right.temp, dest);
					il.add(divf);
				} else {
					throw new Error("Invalid types for DIV");
				}
				break;
		}

		co.code.addAll(il);

		co.temp = dest;
		co.lval = false;
		co.type = left.getType();

		return co;
	}

	/**
	 * Generate code for unary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from child expression
	 * Step 1a: if child is an lval, add a load to get the data
	 * Step 2: generate instruction to perform unary operation
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(UnaryOpNode node, CodeObject expr) {
		
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 2 */
		if (expr.lval) {
			expr = rvalify(expr);
		}

		co.code.addAll(expr.code);

		InstructionList il = new InstructionList();
		String dest = generateTemp(expr.getType().type);	

		switch (expr.getType().type) {
			case PTR:
			case FLOAT:
				Instruction fneg = new FNeg(expr.temp, dest);
				il.add(fneg);
				break;
			case INT:
				Instruction neg = new Neg(expr.temp, dest);
				il.add(neg);
				break;
			default:
				throw new Error("Unknown unary operation");
		}

		co.code.addAll(il);

		co.temp = dest;
		co.lval = false;
		co.type = expr.getType();

		return co;
	}

	/**
	 * Generate code for assignment statements
	 * 
	 * Step 0: create new code object
	 * Step 1: if LHS is a variable, generate a load instruction to get the address into a register
	 * Step 1a: add code from LHS of assignment (make sure it results in an lval!)
	 * Step 2: add code from RHS of assignment
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate store
	 * 
	 * Hint: it is going to be easiest to just generate a store with a 0 immediate
	 * offset, and the complete store address in a register:
	 * 
	 * sw rhs 0(lhs)
	 */
	@Override
	protected CodeObject postprocess(AssignNode node, CodeObject left,
			CodeObject right) {
		
		CodeObject co = new CodeObject();
		/* FILL IN FROM STEP 2 */

		String offset = "0";

		if (left.isVar()) {
			if (left.getSTE().isLocal()) {
				left.temp = "fp";
				offset = left.getSTE().addressToString();
				//InstructionList leftI = generateAddrFromVariable(left);
				//co.code.addAll(leftI);
			}
			else {
				InstructionList leftI = generateAddrFromVariable(left);
				left.temp = leftI.getLast().getDest();
				co.code.addAll(leftI);
			}
			//co.code.add(new Label("here"));
		}

		if (right.lval) {
			right = rvalify(right);
		}

		co.code.addAll(left.code);
		co.code.addAll(right.code);

		String temp;

		//co.code.add(new Label("assign"));

		if (left.getType().type == Scope.InnerType.INT || left.getType().type == Scope.InnerType.PTR) {
			if(right.getType().type == InnerType.FLOAT) {
				right.type = new Scope.Type(InnerType.INT);
				temp = generateTemp(InnerType.INT);
				co.code.add(new FMovi(right.temp, temp));
				right.temp = temp;
			}
			co.code.add(new Sw(right.temp, left.temp, offset));
		}
		else if (left.getType().type == Scope.InnerType.FLOAT){
			if(right.getType().type == InnerType.INT) {
				right.type = new Scope.Type(InnerType.FLOAT);
				temp = generateTemp(InnerType.FLOAT);
				co.code.add(new IMovf(right.temp, temp));
				right.temp = temp;
			}
			co.code.add(new Fsw(right.temp, left.temp, offset));
		}
	
		co.temp = left.temp;
		co.lval = true;

		if (left.getType().type == Scope.InnerType.PTR) {
			co.type = left.getType().getWrappedType();
		}
		else {
			co.type = left.getType();
		}

		return co;
	}



	/**
	 * Add together all the lists of instructions generated by the children
	 */
	@Override
	protected CodeObject postprocess(StatementListNode node,
			List<CodeObject> statements) {
		CodeObject co = new CodeObject();
		//add the code from each individual statement
		for (CodeObject subcode : statements) {
			co.code.addAll(subcode.code);
		}
		co.type = null; //set to null to trigger errors
		return co;
	}
	
	/**
	 * Generate code for read
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from VarNode (make sure it's an lval)
	 * Step 2: generate GetI instruction, storing into temp
	 * Step 3: generate store, to store temp in variable
	 */
	@Override
	protected CodeObject postprocess(ReadNode node, CodeObject var) {
		
		//Step 0
		CodeObject co = new CodeObject();

		//Generating code for read(id)
		assert(var.getSTE() != null); //var had better be a variable

		InstructionList il = new InstructionList();
		switch(node.getType().type) {
			case INT: 
				//Code to generate if INT:
				//geti tmp
				//if var is global: la tmp', <var>; sw tmp 0(tmp')
				//if var is local: sw tmp offset(fp)
				Instruction geti = new GetI(generateTemp(Scope.InnerType.INT));
				il.add(geti);
				InstructionList store = new InstructionList();
				if (var.getSTE().isLocal()) {
					store.add(new Sw(geti.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					store.addAll(generateAddrFromVariable(var));
					store.add(new Sw(geti.getDest(), store.getLast().getDest(), "0"));
				}
				il.addAll(store);
				break;
			case FLOAT:
				//Code to generate if FLOAT:
				//getf tmp
				//if var is global: la tmp', <var>; fsw tmp 0(tmp')
				//if var is local: fsw tmp offset(fp)
				Instruction getf = new GetF(generateTemp(Scope.InnerType.FLOAT));
				il.add(getf);
				InstructionList fstore = new InstructionList();
				if (var.getSTE().isLocal()) {
					fstore.add(new Fsw(getf.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					fstore.addAll(generateAddrFromVariable(var));
					fstore.add(new Fsw(getf.getDest(), fstore.getLast().getDest(), "0"));
				}
				il.addAll(fstore);
				break;
			default:
				throw new Error("Shouldn't read into other variable");
		}
		
		co.code.addAll(il);

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * Generate code for print
	 * 
	 * Step 0: create new code object
	 * 
	 * If printing a string:
	 * Step 1: add code from expression to be printed (make sure it's an lval)
	 * Step 2: generate a PutS instruction printing the result of the expression
	 * 
	 * If printing an integer:
	 * Step 1: add code from the expression to be printed
	 * Step 1a: if it's an lval, generate a load to get the data
	 * Step 2: Generate PutI that prints the temporary holding the expression
	 */
	@Override
	protected CodeObject postprocess(WriteNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		//generating code for write(expr)

		//for strings, we expect a variable
		if (node.getWriteExpr().getType().type == Scope.InnerType.STRING) {
			//Step 1:
			assert(expr.getSTE() != null);
			
			System.out.println("; generating code to print " + expr.getSTE());

			//Get the address of the variable
			InstructionList addrCo = generateAddrFromVariable(expr);
			co.code.addAll(addrCo);

			//Step 2:
			Instruction write = new PutS(addrCo.getLast().getDest());
			co.code.add(write);
		} else {
			//Step 1a:
			//if expr is an lval, load from it
			if (expr.lval == true) {
				expr = rvalify(expr);
			}
			
			//Step 1:
			co.code.addAll(expr.code);

			//Step 2:
			//if type of writenode is int, use puti, if float, use putf
			Instruction write = null;
			switch(node.getWriteExpr().getType().type) {
			case STRING: throw new Error("Shouldn't have a STRING here");
			case INT: 
			case PTR: //should work the same way for pointers
				write = new PutI(expr.temp); break;
			case FLOAT: write = new PutF(expr.temp); break;
			default: throw new Error("WriteNode has a weird type");
			}

			co.code.add(write);
		}

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Generating an instruction sequence for a conditional expression
	 * 
	 * Implement this however you like. One suggestion:
	 *
	 * Create the code for the left and right side of the conditional, but defer
	 * generating the branch until you process IfStatementNode or WhileNode (since you
	 * do not know the labels yet). Modify CodeObject so you can save the necessary
	 * information to generate the branch instruction in IfStatementNode or WhileNode
	 * 
	 * Alternate idea 1:
	 * 
	 * Don't do anything as part of CodeGenerator. Create a new visitor class
	 * that you invoke *within* your processing of IfStatementNode or WhileNode
	 * 
	 * Alternate idea 2:
	 * 
	 * Create the branch instruction in this function, then tweak it as necessary in
	 * IfStatementNode or WhileNode
	 * 
	 * Hint: you may need to preserve extra information in the returned CodeObject to
	 * make sure you know the type of branch code to generate (int vs float)
	 */
	@Override
	protected CodeObject postprocess(CondNode node, CodeObject left, CodeObject right) {
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		if (left.lval) {
			left = rvalify(left);
		}

		if (right.lval) {
			right = rvalify(right);
		}
		

		co.code.addAll(left.code);
		co.code.addAll(right.code);

		String lefttemp = left.temp;
		String righttemp = right.temp;
		CondNode.OpType opType = node.getOp();

		co.temp = lefttemp + " " + righttemp + " " + opType.toString();
		co.type = left.getType();
		co.lval = false;

		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		<cond code>
	 *		<flipped branch> elseLabel
	 *		<then code>
	 *		j outLabel
	 *		elseLabel:
	 *		<else code>
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(IfStatementNode node, CodeObject cond, CodeObject tlist, CodeObject elist) {
		//Step 0:
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		String outLabel = generateOutLabel();
		String elseLabel = elist != null ? generateElseLabel() : outLabel;
		

		co.code.addAll(cond.code);

		String[] parts = cond.temp.split(" ");
		String lefttemp = parts[0];
		String righttemp = parts[1];
		CondNode.OpType op = CondNode.OpType.valueOf(parts[2]);
		//CondNode.OpType reversedOp = op.getReversedOp();

		InstructionList il = new InstructionList();
		String dest = generateTemp(Scope.InnerType.INT);
		String emptyReg = "x0";

		switch (cond.getType().type) {
			case FLOAT:
				switch (op) {
					case EQ:
					Instruction feq = new Feq(lefttemp, righttemp, dest);
					il.add(feq);
					Instruction bne = new Bne(dest, emptyReg, outLabel);
					il.add(bne);
					break;
				case NE:
					Instruction fneq = new Feq(lefttemp, righttemp, dest);
					il.add(fneq);
					Instruction beq = new Beq(dest, emptyReg, outLabel);
					il.add(beq);
					break;
				case LT:
					Instruction fge = new Flt(lefttemp, righttemp, dest);
					il.add(fge);
					Instruction bnelt = new Beq(dest, emptyReg, outLabel);
					il.add(bnelt);
					break;
				case LE:
					// Instruction fgt = new Fgt(lefttemp, righttemp, dest);
					// il.add(fgt);
					Instruction bgtle = new Bgt(dest, emptyReg, outLabel);
					il.add(bgtle);
					break;
				case GT:
					Instruction fle = new Fle(lefttemp, righttemp, dest);
					il.add(fle);
					Instruction bnegt = new Bne(dest, emptyReg, outLabel);
					il.add(bnegt);
					break;
				case GE:
					Instruction flt = new Flt(lefttemp, righttemp, dest);
					il.add(flt);
					Instruction beqge = new Bne(dest, emptyReg, outLabel);
					il.add(beqge);
					break;
					default:
						throw new Error("If statement has wrong type for FLOAT");
				}
				break;
			case INT:
				switch (op) {
					case EQ:
					Instruction bne = new Bne(lefttemp, righttemp, elseLabel);
					il.add(bne);
					break;
				case NE:
					Instruction beq = new Beq(lefttemp, righttemp, elseLabel);
					il.add(beq);
					break;
				case LT:
					Instruction bge = new Bge(lefttemp, righttemp, elseLabel);
					il.add(bge);
					break;
				case LE:
					Instruction bgt = new Bgt(lefttemp, righttemp, elseLabel);
					il.add(bgt);
					break;
				case GT:
					Instruction ble = new Ble(lefttemp, righttemp, elseLabel);
					il.add(ble);
					break;
				case GE:
					Instruction blt = new Blt(lefttemp, righttemp, elseLabel);
					il.add(blt);
					break;
				default:
					throw new Error("If statement has wrong type for INT");
			}
			break;
			default:
				throw new Error("If statement has wrong type");
		}

		co.code.addAll(il);
		co.code.addAll(tlist.code);


		if (elist != null) {
			co.code.add(new J(outLabel));
			co.code.add(new Label(elseLabel));
			co.code.addAll(elist.code);
		}

		co.code.add(new Label(outLabel));

		co.lval = false;

		return co;
	}

		/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		loopLabel:
	 *		<cond code>
	 *		<flipped branch> outLabel
	 *		<body code>
	 *		j loopLabel
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(WhileNode node, CodeObject cond, CodeObject slist) {
		//Step 0:
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 3*/
		String loopLabel = generateLoopLabel();
		String outLabel = generateOutLabel();

		co.code.add(new Label(loopLabel));
		co.code.addAll(cond.code);

		Set<String> loopVariables = collectLoopVariables(slist);

		CodeObject optimizedSlist = new CodeObject();
		for (Instruction instr : slist.code) {
			if (isLoopInvariant(instr, loopVariables)) {
				co.code.add(instr); 
			} else {
				optimizedSlist.code.add(instr); 
			}
		}

		String[] parts = cond.temp.split(" ");
		String lefttemp = parts[0];
		String righttemp = parts[1];
		CondNode.OpType op = CondNode.OpType.valueOf(parts[2]);

		InstructionList il = new InstructionList();

		String dest = generateTemp(Scope.InnerType.INT);
		String emptyReg = "x0";

		switch (cond.getType().type) {
			case FLOAT:
			switch (op) {
				case EQ:
					Instruction feq = new Feq(lefttemp, righttemp, dest);
					il.add(feq);
					Instruction bne = new Bne(dest, emptyReg, outLabel);
					il.add(bne);
					break;
				case NE:
					Instruction fneq = new Feq(lefttemp, righttemp, dest);
					il.add(fneq);
					Instruction beq = new Beq(dest, emptyReg, outLabel);
					il.add(beq);
					break;
				case LT:
					Instruction fge = new Flt(lefttemp, righttemp, dest);
					il.add(fge);
					Instruction bnelt = new Beq(dest, emptyReg, outLabel);
					il.add(bnelt);
					break;
				case LE:
					// Instruction fgt = new Fgt(lefttemp, righttemp, dest);
					// il.add(fgt);
					Instruction bgtle = new Bgt(dest, emptyReg, outLabel);
					il.add(bgtle);
					break;
				case GT:
					Instruction fle = new Fle(lefttemp, righttemp, dest);
					il.add(fle);
					Instruction bnegt = new Bne(dest, emptyReg, outLabel);
					il.add(bnegt);
					break;
				case GE:
					Instruction flt = new Flt(lefttemp, righttemp, dest);
					il.add(flt);
					Instruction beqge = new Bne(dest, emptyReg, outLabel);
					il.add(beqge);
					break;
				default:
					throw new Error("While statement has wrong type for FLOAT");
			}
			break;
			case INT:
				switch (op) {
					case EQ:
					Instruction bne = new Bne(lefttemp, righttemp, outLabel);
					il.add(bne);
					break;
				case NE:
					Instruction beq = new Beq(lefttemp, righttemp, outLabel);
					il.add(beq);
					break;
				case LT:
					Instruction bge = new Bge(lefttemp, righttemp, outLabel);
					il.add(bge);
					break;
				case LE:
					Instruction bgt = new Bgt(lefttemp, righttemp, outLabel);
					il.add(bgt);
					break;
				case GT:
					Instruction ble = new Ble(lefttemp, righttemp, outLabel);
					il.add(ble);
					break;
				case GE:
					Instruction blt = new Blt(lefttemp, righttemp, outLabel);
					il.add(blt);
					break;
				default:
					throw new Error("While statement has wrong type for INT");
			}
			break;
			default:
				throw new Error("While statement has wrong type");
		}

		co.code.addAll(il);

		co.code.addAll(slist.code);
		co.code.add(new J(loopLabel));
		co.code.add(new Label(outLabel));

		co.lval = false;

		return co;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generating code for returns
	 * 
	 * Step 0: Generate new code object
	 * 
	 * Step 1: Add retExpr code to code object (rvalify if necessary)
	 * 
	 * Step 2: Store result of retExpr in appropriate place on stack (fp + 8)
	 * 
	 * Step 3: Jump to out label (use @link{generateFunctionOutLabel()})
	 */
	@Override
	protected CodeObject postprocess(ReturnNode node, CodeObject retExpr) {
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 4 */
		if(retExpr == null){
			co.type = new Scope.Type(InnerType.VOID);
			co.code.add(new J(generateFunctionOutLabel()));	
			return co;
		}
		else if (retExpr.lval){
			retExpr = rvalify(retExpr);
		}

		co.code.addAll(retExpr.code);

		Instruction storeResult;


		switch(retExpr.getType().type) {
			case PTR:
				storeResult = new Sw(retExpr.temp, "fp", "8");
				break;
			case INT:
				storeResult = new Sw(retExpr.temp, "fp", "8");
				break;
			case FLOAT:
				storeResult = new Fsw(retExpr.temp, "fp", "8");
				break;
			default:
				throw new Error("Unsupported return type" + retExpr.getType().type);
		}

		co.code.add(storeResult);

		String out = generateFunctionOutLabel();
		Instruction jumpOut = new J(out);
		co.code.add(jumpOut);

		co.temp = null;
		co.lval = false;
		co.type = null;

		return co;
	}

	@Override
	protected void preprocess(FunctionNode node) {
		// Generate function label information, used for other labels inside function
		currFunc = node.getFuncName();

		//reset register counts; each function uses new registers!
		intRegCount = 0;
		floatRegCount = 0;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generate code for functions
	 * 
	 * Step 1: add the label for the beginning of the function
	 * 
	 * Step 2: manage frame  pointer
	 * 			a. Save old frame pointer
	 * 			b. Move frame pointer to point to base of activation record (current sp)
	 * 			c. Update stack pointer
	 * 
	 * Step 3: allocate new stack frame (use scope infromation from FunctionNode)
	 * 
	 * Step 4: save registers on stack (Can inspect intRegCount and floatRegCount to know what to save)
	 * 
	 * Step 5: add the code from the function body
	 * 
	 * Step 6: add post-processing code:
	 * 			a. Label for `return` statements inside function body to jump to
	 * 			b. Restore registers
	 * 			c. Deallocate stack frame (set stack pointer to frame pointer)
	 * 			d. Reset fp to old location
	 * 			e. Return from function
	 */
	@Override
	protected CodeObject postprocess(FunctionNode node, CodeObject body) {
		CodeObject co = new CodeObject();

		/* FILL IN */
		String functionLabel = generateFunctionLabel(node.getFuncName());
		co.code.add(new Label(functionLabel));

		co.code.add(new Sw("fp", "sp", "0"));
		co.code.add(new Mv("sp", "fp"));
		co.code.add(new Addi("sp", "-4", "sp"));

		LocalScope scope = node.getScope();
		int frameSize = scope.getNumLocals() * 4;

		co.code.add(new Addi("sp", String.valueOf(-frameSize), "sp"));

		for (int i = 1; i <= intRegCount; i++) {
			co.code.add(new Sw("t" + i, "sp", "0"));
			co.code.add(new Addi("sp", "-4", "sp"));
		}

		for (int f = 1; f <= floatRegCount; f++) {
			co.code.add(new Fsw("f" + f, "sp",  "0"));
			co.code.add(new Addi("sp", "-4","sp"));
		}

		co.code.addAll(body.code);
		
		String returnLabel = generateFunctionOutLabel();
		co.code.add(new Label(returnLabel));

		for (int f = floatRegCount; f >= 1; f--) {
			co.code.add(new Addi("sp", "4", "sp"));
			co.code.add(new Flw("f" + f, "sp", "0"));
		}

		for (int i = intRegCount; i >= 1; i--) {
			co.code.add(new Addi("sp", "4", "sp"));
			co.code.add(new Lw("t" + i, "sp", "0"));
		}
		

		co.code.add(new Mv("fp", "sp"));
		co.code.add(new Lw("fp", "sp", "0"));
		//co.code.add(new Addi("sp", "4", "sp"));

		co.code.add(new Ret());
		co.temp = body.temp;

		return co;
	}



	/**
	 * Generate code for the list of functions. This is the "top level" code generation function
	 * 
	 * Step 1: Set fp to point to sp
	 * 
	 * Step 2: Insert a JR to main
	 * 
	 * Step 3: Insert a HALT
	 * 
	 * Step 4: Include all the code of the functions
	 */
	@Override
	protected CodeObject postprocess(FunctionListNode node, List<CodeObject> funcs) {
		CodeObject co = new CodeObject();

		co.code.add(new Mv("sp", "fp"));
		co.code.add(new Jr(generateFunctionLabel("main")));
		co.code.add(new Halt());
		co.code.add(new Blank());

		//add code for each of the functions
		for (CodeObject c : funcs) {
			co.code.addAll(c.code);
			co.code.add(new Blank());
		}

		return co;
	}

	/**
	* 
	* FILL IN FOR STEP 4
	* 
	* Generate code for a call expression
	 * 
	 * Step 1: For each argument:
	 * 
	 * 	Step 1a: insert code of argument (don't forget to rvalify!)
	 * 
	 * 	Step 1b: push result of argument onto stack 
	 * 
	 * Step 2: alloate space for return value
	 * 
	 * Step 3: push current return address onto stack
	 * 
	 * Step 4: jump to function
	 * 
	 * Step 5: pop return address back from stack
	 * 
	 * Step 6: pop return value into fresh temporary (destination of call expression)
	 * 
	 * Step 7: remove arguments from stack (move sp)
	 * 
	 * Add special handling for malloc and free
	 */

	 /**
	  * FOR STEP 6: Make sure to handle VOID functions properly
	  */
	@Override
	protected CodeObject postprocess(CallNode node, List<CodeObject> args) {
		
		//STEP 0
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 4 */
		CodeObject temp = new CodeObject();
		for (CodeObject arg : args) {
			temp = arg;
			if (temp.lval) {
				temp = rvalify(temp);
			}
			co.code.addAll(temp.getCode());
			switch (temp.getType().type) {
				case PTR:
				case INT:
					co.code.add(new Sw(temp.temp, "sp", "0"));
					co.code.add(new Addi("sp", "-4", "sp"));
					break;
				case FLOAT:
					co.code.add(new Fsw(temp.temp, "sp", "0"));
					co.code.add(new Addi("sp", "-4", "sp"));
					break;
				default:
					throw new Error("Unsupported argument type");
			}
		}

		co.code.add(new Addi("sp", "-4", "sp"));

		co.code.add(new Sw("ra", "sp", "0"));
		co.code.add(new Addi("sp", "-4", "sp"));

		String functionLabel = generateFunctionLabel(node.getFuncName());
		co.code.add(new Jr(functionLabel));

		co.code.add(new Addi("sp", "4", "sp"));
		co.code.add(new Lw("ra", "sp", "0"));
		co.code.add(new Addi("sp", "4", "sp"));

		if (node.getType().type != InnerType.VOID) {
			String resultTemp = null;
			switch (node.getType().type) {
				case PTR:
				case INT:
					resultTemp = generateTemp(node.getType().type);
					//co.code.add(new Addi("sp", "4", "sp"));
					co.code.add(new Lw(resultTemp, "sp", "0"));  
					break;
				case FLOAT:
					resultTemp = generateTemp(node.getType().type);
					//co.code.add(new Addi("sp", "4", "sp"));
					co.code.add(new Flw(resultTemp, "sp", "0")); 
					break;
				default:
					throw new Error("Unsupported return type");
			}
			co.temp = resultTemp;
		}

		for (CodeObject arg: args) {
			temp = arg;
			switch(temp.getType().type) {
				case PTR:
				case INT:
					co.code.add(new Addi("sp", "4", "sp"));
					break;
				case FLOAT:
					co.code.add(new Addi("sp", "4", "sp"));
					break;
				default:
					throw new Error("Unsupported type");
			}

		}
		co.type = node.getType();

		return co;
	}	

	
	/**
	 * Generate code for * (expr)
	 * 
	 * Goal: convert the r-val coming from expr (a computed address) into an l-val (an address that can be loaded/stored)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Rvalify expr if needed
	 * 
	 * Step 2: Copy code from expr (including any rvalification) into new code object
	 * 
	 * Step 3: New code object has same temporary as old code, but now is marked as an l-val
	 * 
	 * Step 4: New code object has an "unwrapped" type: if type of expr is * T, type of temporary is T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(PtrDerefNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval){
			expr = rvalify(expr);
		}

		co.code.addAll(expr.getCode());

		co.lval = true;
		co.temp = expr.temp;
		co.type = expr.getType().getWrappedType();	

		return co;
	}

	/**
	 * Generate code for a & (expr)
	 * 
	 * Goal: convert the lval coming from expr (an address) to an r-val (a piece of data that can be used)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: If lval is a variable, generate code to put address into a register (e.g., generateAddressFromVar)
	 *			Otherwise just copy code from other code object
	 * 
	 * Step 2: New code object has same temporary as existing code, but is an r-val
	 * 
	 * Step 3: New code object has a "wrapped" type. If type of expr is T, type of temporary is *T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(AddrOfNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		if(expr.isVar()){
			InstructionList temp = generateAddrFromVariable(expr);
			co.temp = temp.getLast().getDest();
			co.code.addAll(temp);
		} else {
			co.temp = expr.temp;
			co.code.addAll(expr.getCode());		
		}

		co.lval = false;
		co.type = Scope.Type.pointerToType(expr.getType());

		return co;
	}

	/**
	 * Generate code for malloc
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new MALLOC instruction
	 * 
	 * Step 3: Set code object type to INFER
	 */
	@Override
	protected CodeObject postprocess(MallocNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval){
			expr = rvalify(expr);
		}

		co.code.addAll(expr.getCode());

		String temp = generateTemp(expr.getType().type);
		Instruction mallocInst = new Malloc(expr.code.getLast().getDest(),temp);

		co.code.add(mallocInst);
		co.temp = temp; 
		co.type = new Scope.Type(InnerType.INFER);

		return co;
	}
	
	/**
	 * Generate code for free
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new FREE instruction
	 */
	@Override
	protected CodeObject postprocess(FreeNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval){
			expr = rvalify(expr);
		}

		co.code.addAll(expr.getCode());

		Instruction free = new Free(expr.temp);

		co.code.add(free);	
		co.temp = expr.temp;

		return co;
	}

	@Override
	protected CodeObject postprocess(CastNode node, CodeObject expr) {

		//System.err.println("Casting " + expr.getType().type + " to " + node.getType().type);

		CodeObject co = new CodeObject();

		if(expr.lval){
			expr = rvalify(expr);
		}

		co.code.addAll(expr.getCode());
		String dest; 

		co.code.add(new Label("cast " + node.getType().type.toString()));

		if(node.getType().type == Scope.InnerType.FLOAT && expr.getType().type == Scope.InnerType.INT){
			dest = generateTemp(Scope.InnerType.FLOAT);
			co.code.add(new IMovf(expr.temp, dest));
			co.type = new Scope.Type(Scope.InnerType.FLOAT);
		}
		else if(node.getType().type == Scope.InnerType.INT && expr.getType().type == Scope.InnerType.FLOAT){
			dest = generateTemp(Scope.InnerType.INT);
			co.code.add(new FMovi(expr.temp, dest));
			co.type = new Scope.Type(Scope.InnerType.INT);
		}
		else {
			dest = expr.temp;
			co.type = expr.getType();
		}

		co.temp = dest;
		co.lval = false;

		return co;
	}

	private boolean isLoopInvariant(Instruction instr, Set<String> loopVariables) {
			for (String var : loopVariables) {
				if (instr.getClass().toString().contains(var)) {
				return false;
			}
		}
		return true;
	}
	

	private Set<String> collectLoopVariables(CodeObject loopCode) {
		Set<String> vars = new HashSet<>();
		for (Instruction instr : loopCode.code) {
			if (instr.getDest() != null) {
				vars.add(instr.getDest());
			}
		}
		return vars;
	}


	/**
	 * Generate a fresh temporary
	 * 
	 * @return new temporary register name
	 */
	protected String generateTemp(Scope.InnerType t) {
		switch(t) {
			case INT: 
			case PTR: //works the same for pointers
				return intTempPrefix + String.valueOf(++intRegCount);
			case FLOAT: return floatTempPrefix + String.valueOf(++floatRegCount);
			default: throw new Error("Generating temp for bad type");
		}
	}

	protected String generateLoopLabel() {
		return "loop_" + String.valueOf(++loopLabel);
	}

	protected String generateElseLabel() {
		return  "else_" + String.valueOf(++elseLabel);
	}

	protected String generateOutLabel() {
		return "out_" +  String.valueOf(++outLabel);
	}

	protected String generateFunctionLabel() {
		return "func_" + currFunc;
	}

	protected String generateFunctionLabel(String func) {
		return "func_" + func;
	}

	protected String generateFunctionOutLabel() {
		return "func_ret_" + currFunc;
	}
	
	/**
	 * Take a code object that results in an lval, and create a new code
	 * object that adds a load to generate the rval.
	 * 
	 * @param lco The code object resulting in an address
	 * @return A code object with all the code of <code>lco</code> followed by a load
	 *         to generate an rval
	 */
	protected CodeObject rvalify(CodeObject lco) {
		
		assert (lco.lval == true);
		CodeObject co = new CodeObject();

		/* FILL IN FROM STEP 2 */
		InstructionList il = new InstructionList();
		String newTemp = null;
		if (lco.isVar() && lco.getSTE().isLocal()) {
			String address = lco.getSTE().addressToString();
	
			switch (lco.getType().type) {
				case PTR:
					newTemp = generateTemp(lco.getType().type);
					Instruction loadp = new Lw(newTemp, "fp", address);  
					il.add(loadp);
					break;
				case INT:
					newTemp = generateTemp(Scope.InnerType.INT);
					Instruction loadi = new Lw(newTemp, "fp", address);  
					il.add(loadi);
					break;
				case FLOAT:
					newTemp = generateTemp(Scope.InnerType.FLOAT);
					Instruction loadf = new Flw(newTemp, "fp", address); 
					il.add(loadf);
					break;
				default:
					throw new Error("Unsupported type");
			}
		} else {
			if (lco.isVar()) {
				InstructionList leftI = generateAddrFromVariable(lco);
				lco.code.addAll(leftI);
				lco.temp = leftI.getLast().getDest();
			}

			co.code.addAll(lco.code);
			//co.code.add(new Label("here"));
			
			switch(lco.getType().type) {
				case PTR:
					newTemp = generateTemp(lco.getType().type);
					Instruction loadp = new Lw(newTemp, lco.temp, "0");
					il.add(loadp);
					break;
				case INT:
					newTemp = generateTemp(Scope.InnerType.INT);
					Instruction loadi = new Lw(newTemp, lco.temp, "0");
					il.add(loadi);
					break;
				case FLOAT:
					newTemp = generateTemp(Scope.InnerType.FLOAT);
					Instruction loadf = new Flw(newTemp, lco.temp, "0");
					il.add(loadf);
					break;
				default:
					throw new Error("Generating load for bad type");
			}
		}

		co.code.addAll(il);
		co.lval = false;
		co.temp = newTemp;
		co.type = lco.getType();	
		/* DON'T FORGET TO ADD CODE TO GENERATE LOADS FOR LOCAL VARIABLES */

		return co;
	}


	/**
	 * Generate an instruction sequence that holds the address of the variable in a code object
	 * 
	 * If it's a global variable, just get the address from the symbol table
	 * 
	 * If it's a local variable, compute the address relative to the frame pointer (fp)
	 * 
	 * @param lco The code object holding a variable
	 * @return a list of instructions that puts the address of the variable in a register
	 */
	private InstructionList generateAddrFromVariable(CodeObject lco) {

		InstructionList il = new InstructionList();

		//Step 1:
		SymbolTableEntry symbol = lco.getSTE();
		String address = symbol.addressToString();

		//Step 2:
		Instruction compAddr = null;
		if (symbol.isLocal()) {
			//If local, address is offset
			//need to load fp + offset
			//addi tmp' fp offset
			//System.out.println("Generating ADDI for local variable: " + symbol.getName());
			compAddr = new Addi("fp", address, generateTemp(Scope.InnerType.INT));
		} else {
			//If global, address in symbol table is the right location
			//la tmp' addr //Register type needs to be an int
			compAddr = new La(generateTemp(Scope.InnerType.INT), address);
		}
		il.add(compAddr); //add instruction to code object

		return il;
	}

}
