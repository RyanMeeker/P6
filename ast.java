import java.io.*;
import java.util.*;
import java.util.jar.Attributes.Name;

// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a brevis program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of 
// children) or as a fixed set of fields.
//
// The nodes for literals and identifiers contain line and character 
// number information; for string literals and identifiers, they also 
// contain a string; for integer literals, they also contain an integer 
// value.
//
// Here are all the different kinds of AST nodes and what kinds of 
// children they have.  All of these kinds of AST nodes are subclasses
// of "ASTnode".  Indentation indicates further subclassing:
//
//     Subclass              Children
//     --------              --------
//     ProgramNode           DeclListNode
//     DeclListNode          linked list of DeclNode
//     DeclNode:
//       VarDeclNode         TypeNode, IdNode, int
//       FnDeclNode          TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode      TypeNode, IdNode
//       RecordDeclNode      IdNode, DeclListNode
//
//     StmtListNode          linked list of StmtNode
//     ExpListNode           linked list of ExpNode
//     FormalsListNode       linked list of FormalDeclNode
//     FnBodyNode            DeclListNode, StmtListNode
//
//     TypeNode:
//       BoolNode            --- none ---
//       IntNode             --- none ---
//       VoidNode            --- none ---
//       RecordNode          IdNode
//
//     StmtNode:
//       AssignStmtNode      AssignExpNode
//       PostIncStmtNode     ExpNode
//       PostDecStmtNode     ExpNode
//       IfStmtNode          ExpNode, DeclListNode, StmtListNode
//       IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//       WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//       ReadStmtNode        ExpNode
//       WriteStmtNode       ExpNode
//       CallStmtNode        CallExpNode
//       ReturnStmtNode      ExpNode
//
//     ExpNode:
//       TrueNode            --- none ---
//       FalseNode           --- none ---
//       IdNode              --- none ---
//       IntLitNode          --- none ---
//       StrLitNode          --- none ---
//       DotAccessNode       ExpNode, IdNode
//       AssignExpNode       ExpNode, ExpNode
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        ExpNode
//         UnaryMinusNode
//         NotNode
//       BinaryExpNode       ExpNode ExpNode
//         PlusNode     
//         MinusNode
//         TimesNode
//         DivideNode
//         EqualsNode
//         NotEqualsNode
//         LessNode
//         LessEqNode
//         GreaterNode
//         GreaterEqNode
//         AndNode
//         OrNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with linked lists of children, 
// or internal nodes with a fixed number of children:
//
// (1) Leaf nodes:
//        BoolNode,  IntNode,     VoidNode,   TrueNode,  FalseNode,
//        IdNode,    IntLitNode,  StrLitNode
//
// (2) Internal nodes with (possibly empty) linked lists of children:
//        DeclListNode, StmtListNode, ExpListNode, FormalsListNode
//
// (3) Internal nodes with fixed numbers of children:
//        ProgramNode,     VarDeclNode,     FnDeclNode,    FormalDeclNode,
//        RecordDeclNode,  FnBodyNode,      RecordNode,    AssignStmtNode,
//        PostIncStmtNode, PostDecStmtNode, IfStmtNode,    IfElseStmtNode,
//        WhileStmtNode,   ReadStmtNode,    WriteStmtNode, CallStmtNode,
//        ReturnStmtNode,  DotAccessNode,   AssignExpNode, CallExpNode,
//        UnaryExpNode,    UnaryMinusNode,  NotNode,       BinaryExpNode,   
//        PlusNode,        MinusNode,       TimesNode,     DivideNode,
//        EqualsNode,      NotEqualsNode,   LessNode,      LessEqNode,
//        GreaterNode,     GreaterEqNode,   AndNode,       OrNode
//
// **********************************************************************

// **********************************************************************
//   ASTnode class (base class for all other kinds of nodes)
// **********************************************************************


abstract class ASTnode { 
    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void doIndent(PrintWriter p, int indent) {
        for (int k=0; k<indent; k++) p.print(" ");
    }
}

// **********************************************************************
//   ProgramNode,  DeclListNode, StmtListNode, ExpListNode, 
//   FormalsListNode, FnBodyNode
// **********************************************************************

class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
        myDeclList = L;
    }

    /***
     * nameAnalysis
     * Creates an empty symbol table for the outermost scope, then processes
     * all of the globals, record defintions, and functions in the program.
     ***/
    public void nameAnalysis() {
        SymTab symTab = new SymTab();
        myDeclList.nameAnalysis(symTab);
        if (noMain) {
            ErrMsg.fatal(0, 0, "No main function");
        }
    }

    /***
     * typeCheck
     ***/
    public void typeCheck() {
        myDeclList.typeCheck();
    }

    /***
     * codeGen
     ***/
    public void codeGen() {
        myDeclList.codeGen();
    }

    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
    }

    // one child
    private DeclListNode myDeclList;

    public static boolean noMain = true;
}

class DeclListNode extends ASTnode {
    public DeclListNode(List<DeclNode> S) {
        myDecls = S;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, process all of the decls in the list.
     ***/
    public void nameAnalysis(SymTab symTab) {
        nameAnalysis(symTab, symTab);
    }

    public void codeGen(){
        for (DeclNode node : myDecls) {
            if ( !(node instanceof RecordDeclNode) ){
            node.codeGen();
            }
        }
    }
    /***
     * nameAnalysis
     * Given a symbol table symTab and a global symbol table globalTab
     * (for processing record names in variable decls), process all of the 
     * decls in the list.
     ***/    
    public void nameAnalysis(SymTab symTab, SymTab globalTab) {
        for (DeclNode node : myDecls) {
            if (node instanceof VarDeclNode) {
                ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
            } else {
                node.nameAnalysis(symTab);
            }
        }
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck() {
        for (DeclNode node : myDecls) {
            node.typeCheck();
        }
    }
   
    public void unparse(PrintWriter p, int indent) {
        Iterator it = myDecls.iterator();
        try {
            while (it.hasNext()) {
                ((DeclNode)it.next()).unparse(p, indent);
            }
        } catch (NoSuchElementException ex) {
            System.err.println("unexpected NoSuchElementException in DeclListNode.print");
            System.exit(-1);
        }
    }

    // list of children (DeclNodes)
    private List<DeclNode> myDecls;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> S) {
        myStmts = S;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, process each statement in the list.
     ***/
    public void nameAnalysis(SymTab symTab) {
        for (StmtNode node : myStmts) {
            node.nameAnalysis(symTab);
        }
    } 

    public void codeGen() {
        for(StmtNode node: myStmts) {
            node.codeGen();
        }
    }    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        for(StmtNode node : myStmts) {
            node.typeCheck(retType);
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<StmtNode> it = myStmts.iterator();
        while (it.hasNext()) {
            it.next().unparse(p, indent);
        }      
    }

    // list of children (StmtNodes)
    private List<StmtNode> myStmts;
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> S) {
        myExps = S;
    }
    
    public int size() {
        return myExps.size();
    }

    public void codeGen() {
        Iterator<ExpNode> it = myExps.iterator();
        while(it.hasNext()){ // Might need to start at the top of stack instead of bottom.
            it.next().codeGen();
        }
    }    
    /***
     * nameAnalysis
     * Given a symbol table symTab, process each exp in the list.
     ***/
    public void nameAnalysis(SymTab symTab) {
        for (ExpNode node : myExps) {
            node.nameAnalysis(symTab);
        }
    }

    public void unparse(PrintWriter p, int indent) {
        Iterator<ExpNode> it = myExps.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(List<Type> typeList) {
        int k = 0;
        try {
            for (ExpNode node : myExps) {
                Type actualType = node.typeCheck();     // actual type of arg
                
                if (!actualType.isErrorType()) {        // if this is not an error
                    Type formalType = typeList.get(k);  // get the formal type
                    if (!formalType.equals(actualType)) {
                        ErrMsg.fatal(node.lineNum(), node.charNum(),
                                     "Type of actual does not match type of formal");
                    }
                }
                k++;
            }
        } catch (NoSuchElementException e) {
            System.err.println("unexpected NoSuchElementException in ExpListNode.typeCheck");
            System.exit(-1);
        }
    }
    
    // list of children (ExpNodes)
    private List<ExpNode> myExps;
}
class FormalsListNode extends ASTnode {
    public FormalsListNode(List<FormalDeclNode> S) {
        myFormals = S;
    }
    public void codeGen() {
    }
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * for each formal decl in the list
     *     process the formal decl
     *     if there was no error, add type of formal decl to list
     ***/
    public List<Type> nameAnalysis(SymTab symTab) {
        List<Type> typeList = new LinkedList<Type>();
        for (FormalDeclNode node : myFormals) {
            Sym sym = node.nameAnalysis(symTab);
            if (sym != null) {
                typeList.add(sym.getType());
            }
        }
        return typeList;
    }    
    
    /***
     * Return the number of formals in this list.
     ***/
    public int length() {
        return myFormals.size();
    }

    public void unparse(PrintWriter p, int indent) {
        Iterator<FormalDeclNode> it = myFormals.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of children (FormalDeclNodes)
    private List<FormalDeclNode> myFormals;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
        myDeclList = declList;
        myStmtList = stmtList;
    }

    public void codeGen(){
        myStmtList.codeGen();
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the declaration list
     * - process the statement list
     ***/
    public void nameAnalysis(SymTab symTab) {
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
    }
 
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        myStmtList.typeCheck(retType);
    }    
        
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
        myStmtList.unparse(p, indent);
    }

    // two children
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}


// **********************************************************************
// ****  DeclNode and its subclasses
// **********************************************************************

abstract class DeclNode extends ASTnode {
    /***
     * Note: a formal decl needs to return a sym
     ***/
    abstract public Sym nameAnalysis(SymTab symTab);

    // default version of typeCheck for non-function decls
    public void typeCheck() { }
    abstract public void codeGen();
}

class VarDeclNode extends DeclNode {
    public VarDeclNode(TypeNode type, IdNode id, int size) {
        myType = type;
        myId = id;
        mySize = size;
    }

    /***
     * nameAnalysis (overloaded)
     * Given a symbol table symTab, do:
     * if this name is declared void, then error
     * else if the declaration is of a record type, 
     *     lookup type name (globally)
     *     if type name doesn't exist, then error
     * if no errors so far,
     *     if name has already been declared in this scope, then error
     *     else add name to local symbol table     
     *
     * symTab is local symbol table (say, for record field decls)
     * globalTab is global symbol table (for record type names)
     * symTab and globalTab can be the same
     ***/
    public Sym nameAnalysis(SymTab symTab) {
        return nameAnalysis(symTab, symTab);
    }

    public void codeGen() {
        //TODO: possibly add offset
        if(myId.sym().isGlobal()){
            Codegen.generate(".data");
            Codegen.generate(".align 2");
            Codegen.generateLabeled("_" + myId.name(), ".space 4", "");
        }

    }


    public Sym nameAnalysis(SymTab symTab, SymTab globalTab) {
        boolean badDecl = false;
        String name = myId.name();
        Sym sym = null;
        IdNode recordId = null;

        if (myType instanceof VoidNode) {  // check for void type
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        else if (myType instanceof RecordNode) {
            recordId = ((RecordNode)myType).idNode();
            try {
                sym = globalTab.lookupGlobal(recordId.name());
            
                // if the name for the record type is not found, 
                // or is not a record type
                if (sym == null || !(sym instanceof RecordDefSym)) {
                    ErrMsg.fatal(recordId.lineNum(), recordId.charNum(), 
                                "Name of record type invalid");
                    badDecl = true;
                }
                else {
                    recordId.link(sym);
                }
            } catch (SymTabEmptyException ex) {
                System.err.println("Unexpected SymTabEmptyException " +
                                    " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } 
        }
        
        try {
            if (symTab.lookupLocal(name) != null) {
                ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                            "Identifier multiply-declared");
                badDecl = true;            
            }
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
        } 
        
        if (!badDecl) {  // insert into symbol table
            try {
                if (myType instanceof RecordNode) {
                    sym = new RecordSym(recordId);
                }
                else {
                    sym = new Sym(myType.type());
                    if (!globalTab.isGlobalScope()) {
                        int offset = globalTab.getOffset();
                        sym.setOffset(offset);
                        globalTab.setOffset(offset - 4); // vars are integer or boolean
                    } else {
                            sym.setOffset(1);
                    }
                }
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (SymDuplicationException ex) {
                System.err.println("Unexpected SymDuplicationException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (SymTabEmptyException ex) {
                System.err.println("Unexpected SymTabEmptyException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    } 

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.println(";");
    }

    // three children
    private TypeNode myType;
    private IdNode myId;
    private int mySize;  // use value NON_RECORD if this is not a record type

    public static int NON_RECORD = -1;
}

class FnDeclNode extends DeclNode {
    public FnDeclNode(TypeNode type,
                      IdNode id,
                      FormalsListNode formalList,
                      FnBodyNode body) {
        myType = type;
        myId = id;
        myFormalsList = formalList;
        myBody = body;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name has already been declared in this scope, then error
     * else add name to local symbol table
     * in any case, do the following:
     *     enter new scope
     *     process the formals
     *     if this function is not multiply declared,
     *         update symbol table entry with types of formals
     *     process the body of the function
     *     exit scope
     ***/
    public Sym nameAnalysis(SymTab symTab) {
        String name = myId.name();
        FnSym sym = null;
        try {
            if (symTab.lookupLocal(name) != null) {
                ErrMsg.fatal(myId.lineNum(), myId.charNum(),
                            "Identifier multiply-declared");
            }
        
            else { // add function name to local symbol table

                if (name.equals("main")) {
                   ProgramNode.noMain = false; 
                }

                try {
                    sym = new FnSym(myType.type(), myFormalsList.length());
                    symTab.addDecl(name, sym);
                    myId.link(sym);
                } catch (SymDuplicationException ex) {
                    System.err.println("Unexpected SymDuplicationException " +
                                    " in FnDeclNode.nameAnalysis");
                    System.exit(-1);
                } catch (SymTabEmptyException ex) {
                    System.err.println("Unexpected SymTabEmptyException " +
                                    " in FnDeclNode.nameAnalysis");
                    System.exit(-1);
                }
            }
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }

        symTab.setGlobalScope(false);
        symTab.setOffset(4);  // offset of first param        
        symTab.addScope();  // add a new scope for locals and params
        
        // process the formals
        List<Type> typeList = myFormalsList.nameAnalysis(symTab);
        if (sym != null) {
            sym.addFormals(typeList);
            sym.setParamsSize(symTab.getOffset() - 4);
        }
        
        symTab.setOffset(-8);  // offset of first local
        int temp = symTab.getOffset();

        myBody.nameAnalysis(symTab); // process the function body
        
        if (sym != null) {
            sym.setLocalsSize(-1*(symTab.getOffset() - temp));
            off_set = sym.getLocalsSize(); 
        }
        symTab.setGlobalScope(true);

        try {
            symTab.removeScope();  // exit scope
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }
        
        return null;
    } 
       
    /***
     * typeCheck
     ***/
    public void typeCheck() {
        myBody.typeCheck(myType.type());
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.print("(");
        myFormalsList.unparse(p, 0);
        p.println(") {");
        myBody.unparse(p, indent+4);
        p.println("}\n");
    }

    public String getLabel(){
        return label;
    }



    public void codeGen(){
//     # (1) Push the return addr
//     sw	 $ra, 0($sp)
//     subu $sp, $sp, 4
//   # (2) Push the control link
//     sw   $fp, 0($sp)
//     subu $sp, $sp, 4
//   # (3) set the FP
//   # Note: our convention for $sp is that it points to the first unused word of the stack.
//   # The reason for adding 8 is that the unused word and the control link each take 4 bytes
//     addu $fp, $sp, 8
//   # (4) Push space for the locals
//     subu $sp, $sp, <size of locals in bytes>

        Codegen.generate(".text");

        if(myId.isMain()){
            Codegen.generate(".globl main");
            Codegen.genLabel("main");   
            // Codegen.genLabel("__start");
        } 
        else {
            Codegen.genLabel("_" + myId.name());
        }

        Codegen.genPush(Codegen.RA);
        Codegen.genPush(Codegen.FP);
        // int paramSize = ((FnSym)myId.sym()).getParamsSize();

        // Codegen.generate("addu", Codegen.FP, Codegen.SP, 8);
        // Codegen.generate("subu", Codegen.SP, Codegen.SP,  paramSize * 4); // size of locals
        int paramSize = ((FnSym)myId.sym()).getParamsSize();
        // int off = -myId.sym().getOffset();

        Codegen.generate("addu", Codegen.FP, Codegen.SP, paramSize * 4 + 8); // size of param
        Codegen.generate("subu", Codegen.SP, Codegen.SP, off_set * 4); // size of locals
//body
        Codegen.p.println();
//body
        myBody.codeGen();
//exit
// lw   $ra, 0($fp)      # load return address
// move $t0, $fp         # FP holds the address to which we need to restore SP
// lw   $fp, -4($fp)     # restore FP
// move $sp, $t0         # restore SP
// jr   $ra              # return  
        if (myId.isMain()){
            Codegen.genLabel("_main_Exit");
        }

        Codegen.generateIndexed("lw", Codegen.RA, Codegen.FP, -(myFormalsList.length() * 4) );
		Codegen.generate("move",  Codegen.T0, Codegen.FP);
		Codegen.generateIndexed("lw", Codegen.FP, Codegen.FP, ( -(myFormalsList.length() * 4) - 4) );
		Codegen.generate("move", Codegen.SP, Codegen.T0);
		
        if(myId.name().equals("main")) {
			Codegen.generate("li", Codegen.V0, "10");
			Codegen.generate("syscall");
		} 
        else {
            //push?
			Codegen.generate("jr", Codegen.RA);
        }        
        Codegen.p.println();
    }
    // 4 children
    private TypeNode myType;
    private IdNode myId;
    private FormalsListNode myFormalsList;
    private FnBodyNode myBody;
    private int off_set;
    private int param;
    String label;
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
        myType = type;
        myId = id;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this formal is declared void, then error
     * else if this formal is already in the local symble table,
     *     then issue multiply declared error message and return null
     * else add a new entry to the symbol table and return that Sym
     ***/
    public Sym nameAnalysis(SymTab symTab) {
        String name = myId.name();
        boolean badDecl = false;
        Sym sym = null;
        
        if (myType instanceof VoidNode) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        try { 
            if (symTab.lookupLocal(name) != null) {
                ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                            "Identifier multiply-declared");
                badDecl = true;
            }
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in FormalDeclNode.nameAnalysis");
            System.exit(-1);
        } 
        
        if (!badDecl) {  // insert into symbol table
            try {
                int offset = symTab.getOffset();
                sym = new Sym(myType.type());
                sym.setOffset(offset);
                symTab.setOffset(offset + 4); // only integer and boolean formals
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (SymDuplicationException ex) {
                System.err.println("Unexpected SymDuplicationException " +
                                   " in FormalDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (SymTabEmptyException ex) {
                System.err.println("Unexpected SymTabEmptyException " +
                                   " in FormalDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
    }
    public void codeGen() {
        
    }
    // two children
    private TypeNode myType;
    private IdNode myId;
}

class RecordDeclNode extends DeclNode {
    public RecordDeclNode(IdNode id, DeclListNode declList) {
        myId = id;
        myDeclList = declList;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name is already in the symbol table,
     *     then multiply declared error (don't add to symbol table)
     * create a new symbol table for this record definition
     * process the decl list
     * if no errors
     *     add a new entry to symbol table for this record
     ***/
    public Sym nameAnalysis(SymTab symTab) {
        String name = myId.name();
        boolean badDecl = false;
        try {
            if (symTab.lookupLocal(name) != null) {
                ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                            "Identifier multiply-declared");
                badDecl = true;            
            }
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in RecordDeclNode.nameAnalysis");
            System.exit(-1);
        } 

        SymTab recordSymTab = new SymTab();
        
        // process the fields of the record
        myDeclList.nameAnalysis(recordSymTab, symTab);
        
        if (!badDecl) {
            try {   // add entry to symbol table
                RecordDefSym sym = new RecordDefSym(recordSymTab);
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (SymDuplicationException ex) {
                System.err.println("Unexpected SymDuplicationException " +
                                   " in RecordDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (SymTabEmptyException ex) {
                System.err.println("Unexpected SymTabEmptyException " +
                                   " in RecordDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return null;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("record ");
        p.print(myId.name());
        p.println("(");
        myDeclList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println(");\n");
    }
    public void codeGen() {
        // Do not need to implement
        System.out.println("Should not implelemnt record codeGen");    
    }
    // two children
    private IdNode myId;
    private DeclListNode myDeclList;
}

// **********************************************************************
// ****  TypeNode and its subclasses
// **********************************************************************

abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method */
    abstract public Type type();
}

class BoolNode extends TypeNode {
    public BoolNode() {
    }

    /***
     * type
     ***/
    public Type type() {
        return new BoolType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("boolean");
    }
}

class IntNode extends TypeNode {
    public IntNode() {
    }

    /***
     * type
     ***/
    public Type type() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("integer");
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }
    
    /***
     * type
     ***/
    public Type type() {
        return new VoidType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("void");
    }
}

class RecordNode extends TypeNode {
    public RecordNode(IdNode id) {
        myId = id;
    }
 
    public IdNode idNode() {
        return myId;
    }
       
    /***
     * type
     ***/
    public Type type() {
        return new RecordType(myId);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("record ");
        p.print(myId.name());
    }
    
    // one child
    private IdNode myId;
}

// **********************************************************************
// ****  StmtNode and its subclasses
// **********************************************************************

abstract class StmtNode extends ASTnode {
    abstract public void nameAnalysis(SymTab symTab);
    abstract public void typeCheck(Type retType);
    abstract public void codeGen();
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignExpNode assign) {
        myAssign = assign;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myAssign.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        myAssign.typeCheck();
    }
            
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myAssign.unparse(p, -1); // no parentheses
        p.println(";");
    }
    public void codeGen(){
        System.out.println("Assign codeGen");

        myAssign.codeGen();
        Codegen.genPop(Codegen.T0);
    }
    // one child
    private AssignExpNode myAssign;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
            
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("++;");
    }
    public void codeGen(){

        ((IdNode)myExp).getAddress(); // do we need this? 

        ((IdNode)myExp).codeGen();

        Codegen.genPop(Codegen.T0);
        Codegen.genPop(Codegen.T1);

        Codegen.generate("addi", Codegen.T0, Codegen.T0, 1);
        Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);

        Codegen.p.println();
    }
    // one child
    private ExpNode myExp;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
            
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("--;");
    }
    public void codeGen(){

        ((IdNode)myExp).getAddress(); // do we need this? 

        ((IdNode)myExp).codeGen();

        Codegen.genPop(Codegen.T0);
        Codegen.genPop(Codegen.T1);

        Codegen.generate("addi", Codegen.T0, Codegen.T0, -1);
        Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);

        Codegen.p.println();
    }
    // one child
    private ExpNode myExp;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myDeclList = dlist;
        myExp = exp;
        myStmtList = slist;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    public void codeGen(){
        String label = Codegen.nextLabel();

        myExp.codeGen();

        Codegen.genPop(Codegen.V0);
        Codegen.generate("li", Codegen.V1, "0");
        Codegen.generate("beq", Codegen.V0, Codegen.V1, label);

        myStmtList.codeGen();

        Codegen.genLabel(label);
    }    
     /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-boolean expression used as if condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
           
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");        
    }

    // three children
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode dlist1,
                          StmtListNode slist1, DeclListNode dlist2,
                          StmtListNode slist2) {
        myExp = exp;
        myThenDeclList = dlist1;
        myThenStmtList = slist1;
        myElseDeclList = dlist2;
        myElseStmtList = slist2;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts of then
     * - exit the scope
     * - enter a new scope
     * - process the decls and stmts of else
     * - exit the scope
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myThenDeclList.nameAnalysis(symTab);
        myThenStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
        symTab.addScope();
        myElseDeclList.nameAnalysis(symTab);
        myElseStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    public void codeGen(){
        // String nextlabel = Codegen.nextLabel();
        // String nextnextlabel = Codegen.nextLabel();

        // myExp.codeGen();

        // Codegen.genPop(Codegen.V0);
        // Codegen.generate("li", Codegen.V1, "0");
        // Codegen.generate("beq", Codegen.V0, Codegen.V1, nextnextlabel);
        // myThenStmtList.codeGen();
        // Codegen.generate("j", nextlabel);

        // // TODO: 
        // Codegen.genLabel(nextlabel);
        // myElseStmtList.codeGen();
        // Codegen.genLabel(nextlabel);

        String ifLabel = Codegen.nextLabel();
        String elseLabel = Codegen.nextLabel();

        myExp.codeGen();
        Codegen.genPop(Codegen.T0);
        Codegen.generate("beq", Codegen.T0, "0", ifLabel);
        myThenStmtList.codeGen();
        Codegen.generate("b", elseLabel);
        Codegen.genLabel(ifLabel);
        myElseStmtList.codeGen();
        Codegen.genLabel(elseLabel);
    }    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-boolean expression used as if condition");        
        }
        
        myThenStmtList.typeCheck(retType);
        myElseStmtList.typeCheck(retType);
    }
            
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myThenDeclList.unparse(p, indent+4);
        myThenStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
        doIndent(p, indent);
        p.println("else {");
        myElseDeclList.unparse(p, indent+4);
        myElseStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}"); 
    }

    // five children
    private ExpNode myExp;
    private DeclListNode myThenDeclList;
    private StmtListNode myThenStmtList;
    private StmtListNode myElseStmtList;
    private DeclListNode myElseDeclList;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myExp = exp;
        myDeclList = dlist;
        myStmtList = slist;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    public void codeGen(){
        String label = Codegen.nextLabel();
        String nextLabel = Codegen.nextLabel();
        Codegen.p.println(label + ": ");

        myExp.codeGen();
        Codegen.genPop(Codegen.V0);
        Codegen.generate("li",Codegen.V1, "0");

        
        Codegen.generate("beq", Codegen.V0, "$zero", nextLabel);

        myStmtList.codeGen();

        Codegen.generate("j", label);
        Codegen.p.println(nextLabel + ": ");

        // String startLabel = Codegen.nextLabel();    // generate unique label for start of loop
        // String endLabel = Codegen.nextLabel();      // generate unique label for end of loop
        
        // Codegen.p.println(startLabel + ":");        // output start label
        // myExp.codeGen();                            // generate code for condition
        // Codegen.genPop(Codegen.T0);                 // pop the top value of the stack into register T0
        
        // Codegen.generate("beq", Codegen.T0, "$zero", endLabel);    // if T0 equals 0, branch to end of loop
        // myStmtList.codeGen();                       // generate code for statements in loop
        // Codegen.generate("j", startLabel);           // branch to start of loop
        
        // Codegen.p.println(endLabel + ":");           // output end label
    

    }    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-boolean expression used as while condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
                
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("while (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }

    // three children
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class ReadStmtNode extends StmtNode {
    public ReadStmtNode(ExpNode e) {
        myExp = e;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
    }    
 
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a function");
        }
        
        if (type.isRecordDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a record name");
        }
        
        if (type.isRecordType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a record variable");
        }
    }

    public void codeGen() {
        String label = Codegen.nextLabel();

        Codegen.generate("li", Codegen.V0, "5");
        Codegen.generate("syscall");
        Codegen.generate("beq", Codegen.V0, Codegen.V1, label);
        Codegen.generate("li", Codegen.V0, Codegen.TRUE);

        Codegen.p.println(label + ": ");

        if(!((IdNode)myExp).sym().isGlobal()){
            Codegen.generateIndexed("sw", Codegen.V0, Codegen.FP, ((IdNode)myExp).sym().getOffset());
        }
        else{
            Codegen.generate("sw", Codegen.V0, "_" + ((IdNode)myExp).name());
        }
        Codegen.p.println();
    }        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("scan -> ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // one child (actually can only be an IdNode or an ArrayExpNode)
    private ExpNode myExp;
}

class WriteStmtNode extends StmtNode {
    public WriteStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
    }

    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        myType = type;
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write to a function");
        }
        
        if (type.isRecordDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write to a record name");
        }
        
        if (type.isRecordType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write to a record variable");
        }
        
        if (type.isVoidType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write to void");
        }
    }
            
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("print <- ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // Call the codeGen method of the expression being printed. That method will generate code to evaluate the expression, leaving that value on the top of the stack.
    // Generate code to pop the top-of-stack value into register A0 (a special register used for output of strings and ints)
    // Generate code to set register V0 to 1.
    // Generate a syscall instruction.
    public void codeGen(){
        System.out.println("write codeGen");

        myExp.codeGen(); 

        if(myExp instanceof StringLitNode) {
            System.out.println("writestmtnode: string");
            Codegen.generate("li", Codegen.V0, 4);
            Codegen.genPop(Codegen.A0);
        }
        else{
            System.out.println("writestmtnode: int");
            Codegen.generate("li", Codegen.V0, 1);
            Codegen.genPop(Codegen.A0); //TODO: check 4
        }

        Codegen.generate("syscall");
        Codegen.p.println();	
    }

    // two children
    private ExpNode myExp;
    private Type myType;
}

class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
        myCall = call;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myCall.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        myCall.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myCall.unparse(p, indent);
        p.println(";");
    }
    public void codeGen() {
        myCall.codeGen();
        Codegen.genPop(Codegen.T1);
    }
    // one child
    private CallExpNode myCall;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child,
     * if it has one
     ***/
    public void nameAnalysis(SymTab symTab) {
        if (myExp != null) {
            myExp.nameAnalysis(symTab);
        }
    }

    //empty codegen for abstract - will use the one with param
    public void codeGen(){

        if(myExp != null && !myExp.typeCheck().isVoidType()){
            myExp.codeGen();
            Codegen.genPop(Codegen.V0);
            ((IdNode)myExp).genJumpLink();
        } 

        // Codegen.generate("b", "_testRet");

        // if(myExp != null && myExp instanceof StringLitNode){
        //     Codegen.generate(null, null, 0);
        // }

        // Codegen.generate("b", endFnLabel);

    }

    // public void codeGen(String endFnLabel){
    //     // Evaluate each actual param, pushing values onto stack
    //     // jump to called function, leaving return address in the RA register
    //     // push the returned value(either V0 or FO) onto stack

    //     System.out.println("returnstmtnode");

    //     if(myExp != null && !myExp.typeCheck().isVoidType()){
    //         myExp.codeGen();
    //         Codegen.genPop(Codegen.V0);
    //     }

    //     Codegen.generate("b", endFnLabel);

    //     // if(myExp instanceof StringLitNode){
    //     //     Codegen.genPop(Codegen.V0);
    //     // }
    //     // else {
    //     //     Codegen.genPop(Codegen.V0);
    //     // }
        
    // }

    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        if (myExp != null) {  // return value given
            Type type = myExp.typeCheck();
            
            if (retType.isVoidType()) {
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Return with a value in a void function");                
            }
            
            else if (!retType.isErrorType() && !type.isErrorType() && !retType.equals(type)){
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Bad return value");
            }
        }
        
        else {  // no return value given -- ok if this is a void function
            if (!retType.isVoidType()) {
                ErrMsg.fatal(0, 0, "Missing return value");                
            }
        }
        
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("return");
        if (myExp != null) {
            p.print(" ");
            myExp.unparse(p, 0);
        }
        p.println(";");
    }

    // one child
    private ExpNode myExp; // possibly null
}

// **********************************************************************
// ****  ExpNode and its subclasses
// **********************************************************************

abstract class ExpNode extends ASTnode {
    /***
     * Default version for nodes with no names
     ***/
    public void nameAnalysis(SymTab symTab) { }
    abstract public void codeGen();
    abstract public Type typeCheck();
    abstract public int lineNum();
    abstract public int charNum();
}

class TrueNode extends ExpNode {
    public TrueNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }
    public void codeGen(){
        Codegen.generate("li", Codegen.V0, 1);
        Codegen.genPush(Codegen.V0);
    }    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new BoolType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("true");
    }

    private int myLineNum;
    private int myCharNum;
}

class FalseNode extends ExpNode {
    public FalseNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    public void codeGen(){
        Codegen.generate("li", Codegen.V0, 0);
        Codegen.genPush(Codegen.V0);
    }    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }

    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new BoolType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("false");
    }

    private int myLineNum;
    private int myCharNum;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }

    /***
     * Link the given symbol to this ID.
     ***/
    public void link(Sym sym) {
        mySym = sym;
    }
    
    /***
     * Return the name of this ID.
     ***/
    public String name() {
        return myStrVal;
    }
    
    /***
     * Return the symbol associated with this ID.
     ***/
    public Sym sym() {
        return mySym;
    }
    
    /***
     * Return the line number for this ID.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this ID.
     ***/
    public int charNum() {
        return myCharNum;
    }    
 
    /***
     * Return the total number of bytes for all local variables.
     * HINT: This method may be useful during code generation.
     ***/
    public int localsSize() {
        if(!(mySym instanceof FnSym)) {
            throw new IllegalStateException("cannot call local size on a non-function");
        }
        return ((FnSym)mySym).getLocalsSize();
    }    

    /***
     * Return the total number of bytes for all parameters.
     * HINT: This method may be useful during code generation.
     ***/
    public int paramsSize() {
        if(!(mySym instanceof FnSym)) {
            throw new IllegalStateException("cannot call local size on a non-function");
        }
        return ((FnSym)mySym).getParamsSize();
    }   

    /***
     * Is this function main?
     * HINT: This may be useful during code generation.
     ***/
    public boolean isMain() {
        return (myStrVal.equals("main"));
    }

    public void getAddress() {
        if (mySym.isGlobal()) {
            Codegen.generate("la", Codegen.T0, "_" + myStrVal);
        } else {
            Codegen.generateIndexed("la", Codegen.T0, Codegen.FP, mySym.getOffset());
        }
        Codegen.genPush(Codegen.T0);
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - check for use of undeclared name
     * - if ok, link to symbol table entry
     ***/
    public void nameAnalysis(SymTab symTab) {
        try {
            Sym sym = symTab.lookupGlobal(myStrVal);
            if (sym == null) {
                ErrMsg.fatal(myLineNum, myCharNum, "Identifier undeclared");
            } else {
                link(sym);
            }
        } catch (SymTabEmptyException ex) {
            System.err.println("Unexpected SymTabEmptyException " +
                               " in IdNode.nameAnalysis");
            System.exit(-1);
        } 
    }
 
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        if (mySym != null) {
            return mySym.getType();
        } 
        else {
            System.err.println("ID with null sym field in IdNode.typeCheck");
            System.exit(-1);
        }
        return null;
    }
               
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
        if (mySym != null) {
            p.print("[" + mySym + "]");
        }
    }

    public void genJumpLink(){
        // System.out.println("myStrVal: " + myStrVal);
        if(myStrVal.equals("main")){
            //generate label
            Codegen.generate("jal", myStrVal);
        } else {
            //generate label
            Codegen.generate("jal", "_" + myStrVal);
        }
    }

    public void codeGen() {
        System.out.println("IdNode codeGen");

        if(mySym.isGlobal()) {
            Codegen.generate("lw", Codegen.V0, "_" + myStrVal);
            Codegen.genPush(Codegen.V0);
            Codegen.p.println();        
        }
        else {
            Codegen.generateIndexed("lw", Codegen.V0, Codegen.FP, mySym.getOffset());
            Codegen.genPush(Codegen.V0);
            Codegen.p.println();
        }
    }
    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    private Sym mySym;
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myIntVal = intVal;
    }
    
    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }
        
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print(myIntVal);
    }
    public void codeGen(){
        System.out.println("IntListNode codeGen");

        Codegen.generate("li", Codegen.V0, myIntVal); //TODO: Does this need to be a string
        Codegen.genPush(Codegen.V0);
    }
    private int myLineNum;
    private int myCharNum;
    private int myIntVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }

    public String name(){
        return myStrVal;
    }
    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new StringType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
    }

    public void codeGen(){
        System.out.println("StringLit Node codeGen");

        String label = Codegen.nextLabel();
        Codegen.generate(".data");
        Codegen.generateLabeled(label, ".asciiz", "", " " + myStrVal);
        
        Codegen.generate(".text");
        Codegen.generate("la", Codegen.T0, label); //TODO: look into this
        Codegen.genPush(Codegen.T0);

        // Codegen.genPop(Codegen.A0);
        // Codegen.generate("li", Codegen.V0, "4");
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
}

class DotAccessExpNode extends ExpNode {
    public DotAccessExpNode(ExpNode loc, IdNode id) {
        myLoc = loc;    
        myId = id;
    }
    public void codeGen() { }
    /***
     * Return the symbol associated with this dot-access node.
     ***/
    public Sym sym() {
        return mySym;
    }    
    
    /***
     * Return the line number for this dot-access node. 
     * The line number is the one corresponding to the RHS of the dot-access.
     ***/
    public int lineNum() {
        return myId.lineNum();
    }
    
    /***
     * Return the char number for this dot-access node.
     * The char number is the one corresponding to the RHS of the dot-access.
     ***/
    public int charNum() {
        return myId.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the LHS of the dot-access
     * - process the RHS of the dot-access
     * - if the RHS is of a record type, set the sym for this node so that
     *   a dot-access "higher up" in the AST can get access to the symbol
     *   table for the appropriate record definition
     ***/
    public void nameAnalysis(SymTab symTab) {
        badAccess = false;
        SymTab recordSymTab = null; // to lookup RHS of dot-access
        Sym sym = null;
        
        myLoc.nameAnalysis(symTab);  // do name analysis on LHS
        
        // if myLoc is really an ID, then sym will be a link to the ID's symbol
        if (myLoc instanceof IdNode) {
            IdNode id = (IdNode)myLoc;
            sym = id.sym();
            
            // check ID has been declared to be of a record type
            
            if (sym == null) { // ID was undeclared
                badAccess = true;
            }
            else if (sym instanceof RecordSym) { 
                // get symbol table for record type
                Sym tempSym = ((RecordSym)sym).getRecordType().sym();
                recordSymTab = ((RecordDefSym)tempSym).getSymTab();
            } 
            else {  // LHS is not a record type
                ErrMsg.fatal(id.lineNum(), id.charNum(), 
                             "Dot-access of non-record type");
                badAccess = true;
            }
        }
        
        // if myLoc is really a dot-access (i.e., myLoc was of the form
        // LHSloc.RHSid), then sym will either be
        // null - indicating RHSid is not of a record type, or
        // a link to the Sym for the record type RHSid was declared to be
        else if (myLoc instanceof DotAccessExpNode) {
            DotAccessExpNode loc = (DotAccessExpNode)myLoc;
            
            if (loc.badAccess) {  // if errors in processing myLoc
                badAccess = true; // don't continue proccessing this dot-access
            }
            else { //  no errors in processing myLoc
                sym = loc.sym();

                if (sym == null) {  // no record in which to look up RHS
                    ErrMsg.fatal(loc.lineNum(), loc.charNum(), 
                                 "Dot-access of non-record type");
                    badAccess = true;
                }
                else {  // get the record's symbol table in which to lookup RHS
                    if (sym instanceof RecordDefSym) {
                        recordSymTab = ((RecordDefSym)sym).getSymTab();
                    }
                    else {
                        System.err.println("Unexpected Sym type in DotAccessExpNode");
                        System.exit(-1);
                    }
                }
            }

        }
        
        else { // don't know what kind of thing myLoc is
            System.err.println("Unexpected node type in LHS of dot-access");
            System.exit(-1);
        }
        
        // do name analysis on RHS of dot-access in the record's symbol table
        if (!badAccess) {
            try {
                sym = recordSymTab.lookupGlobal(myId.name()); // lookup
                if (sym == null) { // not found - RHS is not a valid field name
                    ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                                "Record field name invalid");
                    badAccess = true;
                }
            
                else {
                    myId.link(sym);  // link the symbol
                    // if RHS is itself as record type, link the symbol for its record 
                    // type to this dot-access node (to allow chained dot-access)
                    if (sym instanceof RecordSym) {
                        mySym = ((RecordSym)sym).getRecordType().sym();
                    }
                }
            } catch (SymTabEmptyException ex) {
                System.err.println("Unexpected SymTabEmptyException " +
                                " in DotAccessExpNode.nameAnalysis");
                System.exit(-1);
            } 
        }
    }    
 
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return myId.typeCheck();
    }
        
    // **** unparse ****
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myLoc.unparse(p, 0);
        p.print(").");
        myId.unparse(p, 0);
    }

    // two children
    private ExpNode myLoc;    
    private IdNode myId;
    private Sym mySym;          // link to Sym for record type
    private boolean badAccess;  // to prevent multiple, cascading errors
}

class AssignExpNode extends ExpNode {
    public AssignExpNode(ExpNode lhs, ExpNode exp) {
        myLhs = lhs;
        myExp = exp;
    }
    
    /***
     * Return the line number for this assignment node. 
     * The line number is the one corresponding to the left operand.
     ***/
    public int lineNum() {
        return myLhs.lineNum();
    }
    
    /***
     * Return the char number for this assignment node.
     * The char number is the one corresponding to the left operand.
     ***/
    public int charNum() {
        return myLhs.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     ***/
    public void nameAnalysis(SymTab symTab) {
        myLhs.nameAnalysis(symTab);
        myExp.nameAnalysis(symTab);
    }
  
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type typeLhs = myLhs.typeCheck();
        Type typeExp = myExp.typeCheck();
        Type retType = typeLhs;
        
        if (typeLhs.isFnType() && typeExp.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Function assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isRecordDefType() && typeExp.isRecordDefType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Record name assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isRecordType() && typeExp.isRecordType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Record variable assignment");
            retType = new ErrorType();
        }        
        
        if (!typeLhs.equals(typeExp) && !typeLhs.isErrorType() && !typeExp.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Type mismatch");
            retType = new ErrorType();
        }
        
        if (typeLhs.isErrorType() || typeExp.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
    public void codeGen(){
        myExp.codeGen();

        Codegen.genPop(Codegen.V0);
        
        // handel lhs
        if( !( ((IdNode)myLhs).sym().isGlobal() ) ) {
                Codegen.generateIndexed("sw", Codegen.V0, Codegen.FP, ((IdNode)myLhs).sym().getOffset());
        } else {
            Codegen.generate("sw", Codegen.V0, "_" + ((IdNode)myLhs).name()); 
        }

    }       
    public void unparse(PrintWriter p, int indent) {
        if (indent != -1)  p.print("(");
        myLhs.unparse(p, 0);
        p.print(" = ");
        myExp.unparse(p, 0);
        if (indent != -1)  p.print(")");       
    }

    // two children
    private ExpNode myLhs;
    private ExpNode myExp;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
        myId = name;
        myExpList = elist;
    }

    public CallExpNode(IdNode name) {
        myId = name;
        myExpList = new ExpListNode(new LinkedList<ExpNode>());
    }

    /***
     * Return the line number for this call node. 
     * The line number is the one corresponding to the function name.
     ***/
    public int lineNum() {
        return myId.lineNum();
    }
    
    /***
     * Return the char number for this call node.
     * The char number is the one corresponding to the function name.
     ***/
    public int charNum() {
        return myId.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     ***/
    public void nameAnalysis(SymTab symTab) {
        myId.nameAnalysis(symTab);
        myExpList.nameAnalysis(symTab);
    }    

    public void codeGen(){
        System.out.println("entering CallexpNode codeGen");
        myExpList.codeGen();
        myId.genJumpLink();
        if(!((FnSym)myId.sym()).getReturnType().isVoidType())
        {
            Codegen.genPush(Codegen.V0);
        }   
        
    }      
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        if (!myId.typeCheck().isFnType()) {  
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Attempt to call a non-function");
            return new ErrorType();
        }
        
        FnSym fnSym = (FnSym)(myId.sym());
        
        if (fnSym == null) {
            System.err.println("null sym for Id in CallExpNode.typeCheck");
            System.exit(-1);
        }
        
        if (myExpList.size() != fnSym.getNumParams()) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Function call with wrong number of args");
            return fnSym.getReturnType();
        }
        
        myExpList.typeCheck(fnSym.getParamTypes());
        return fnSym.getReturnType();
    }
            
    public void unparse(PrintWriter p, int indent) {
        myId.unparse(p, 0);
        p.print("(");
        if (myExpList != null) {
            myExpList.unparse(p, 0);
        }
        p.print(")");       
    }

    // two children
    private IdNode myId;
    private ExpListNode myExpList;  // possibly null
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
        myExp = exp;
    }
    
    /***
     * Return the line number for this unary expression node. 
     * The line number is the one corresponding to the  operand.
     ***/
    public int lineNum() {
        return myExp.lineNum();
    }
    
    /***
     * Return the char number for this unary expression node.
     * The char number is the one corresponding to the  operand.
     ***/
    public int charNum() {
        return myExp.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    // one child
    protected ExpNode myExp;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
        myExp1 = exp1;
        myExp2 = exp2;
    }
    
    /***
     * Return the line number for this binary expression node. 
     * The line number is the one corresponding to the left operand.
     ***/
    public int lineNum() {
        return myExp1.lineNum();
    }
    
    /***
     * Return the char number for this binary expression node.
     * The char number is the one corresponding to the left operand.
     ***/
    public int charNum() {
        return myExp1.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     ***/
    public void nameAnalysis(SymTab symTab) {
        myExp1.nameAnalysis(symTab);
        myExp2.nameAnalysis(symTab);
    }
    
    // two children
    protected ExpNode myExp1;
    protected ExpNode myExp2;
}

// **********************************************************************
// *****  Subclasses of UnaryExpNode
// **********************************************************************

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
        super(exp);
    }

    public void codeGen() {
        myExp.codeGen();
        Codegen.genPop(Codegen.V0);
        Codegen.generate("li", Codegen.V1, "0");
        Codegen.generate("sub", Codegen.V0, Codegen.V1, Codegen.V0);
        Codegen.genPush(Codegen.V0);
    }

    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new IntType();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(-");
        myExp.unparse(p, 0);
        p.print(")");
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
        super(exp);
    }

    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new BoolType();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Logical operator applied to non-boolean operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void codeGen() {
        myExp.codeGen();
        Codegen.genPop(Codegen.V0);
        Codegen.generate("li", Codegen.V1, "0");
        Codegen.generate("seq", Codegen.V0, Codegen.V0, Codegen.V1);
        Codegen.genPush(Codegen.V0);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(\\");
        myExp.unparse(p, 0);
        p.print(")");
    }
}

// **********************************************************************
// ****  Subclasses of BinaryExpNode
// **********************************************************************

abstract class ArithmeticExpNode extends BinaryExpNode {
    public ArithmeticExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new IntType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class LogicalExpNode extends BinaryExpNode {
    public LogicalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isBoolType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Logical operator applied to non-boolean operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isBoolType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Logical operator applied to non-boolean operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class EqualityExpNode extends BinaryExpNode {
    public EqualityExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (type1.isVoidType() && type2.isVoidType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to void function calls");
            retType = new ErrorType();
        }
        
        if (type1.isFnType() && type2.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to function names");
            retType = new ErrorType();
        }
        
        if (type1.isRecordDefType() && type2.isRecordDefType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to record names");
            retType = new ErrorType();
        }
        
        if (type1.isRecordType() && type2.isRecordType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to record variables");
            retType = new ErrorType();
        }        
        
        if (!type1.equals(type2) && !type1.isErrorType() && !type2.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Type mismatch");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class RelationalExpNode extends BinaryExpNode {
    public RelationalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

class PlusNode extends ArithmeticExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" + ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);
        Codegen.generate("add", Codegen.V0, Codegen.V0, Codegen.V1);
        Codegen.genPush(Codegen.V0);
        Codegen.p.println();
    }
}

class MinusNode extends ArithmeticExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" - ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);
        Codegen.generate("sub", Codegen.V0, Codegen.V0, Codegen.V1);
        Codegen.genPush(Codegen.V0);
        Codegen.p.println();
    }


}

class TimesNode extends ArithmeticExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" * ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);
        Codegen.generate("mult", Codegen.V0, Codegen.V1);
        Codegen.generate("mflo", Codegen.V0);
        Codegen.genPush(Codegen.V0);
        Codegen.p.println();
    }
}

class DivideNode extends ArithmeticExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" / ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);
        Codegen.generate("div", Codegen.V0, Codegen.V1);
        Codegen.generate("mflo", Codegen.V0);
        Codegen.genPush(Codegen.V0);
        Codegen.p.println();
    }

}

class EqualsNode extends EqualityExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" == ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        if (myExp1 instanceof StringLitNode && myExp2 instanceof StringLitNode){

            if(((StringLitNode) myExp2).toString().equals(((StringLitNode) myExp1).toString())){
                Codegen.generate("li", Codegen.V0, "1");
            } else{
                Codegen.generate("li", Codegen.V0, "0");
            }
            Codegen.genPush(Codegen.V0);
            return;
        }

        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);

        Codegen.generate("seq", Codegen.V0, Codegen.V0, Codegen.V1);

        Codegen.genPush(Codegen.V0);
    }
}

class NotEqualsNode extends EqualityExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" \\= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        if (myExp1 instanceof StringLitNode && myExp2 instanceof StringLitNode){
            String string1 = ((StringLitNode) myExp1).toString();
            String string2 = ((StringLitNode) myExp2).toString();

            if(string1.equals(string2)){
                Codegen.generate("li", Codegen.V0, "0");
            } else{
                Codegen.generate("li", Codegen.V0, "1");
            }
            Codegen.genPush(Codegen.V0);
            return;
        }

        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);

        Codegen.generate("sne", Codegen.V0, Codegen.V0, Codegen.V1);

        Codegen.genPush(Codegen.V0);
    }

}

class LessNode extends RelationalExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" < ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);

        Codegen.generate("slt", Codegen.V0, Codegen.V0, Codegen.V1);

        Codegen.genPush(Codegen.V0);
    }

}

class LessEqNode extends RelationalExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" <= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);

        Codegen.generate("sle", Codegen.V0, Codegen.V0, Codegen.V1);

        Codegen.genPush(Codegen.V0);
    }
}

class GreaterNode extends RelationalExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);

        Codegen.generate("sgt", Codegen.V0, Codegen.V0, Codegen.V1);

        Codegen.genPush(Codegen.V0);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" > ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
}

class GreaterEqNode extends RelationalExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" >= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.V1);
        Codegen.genPop(Codegen.V0);

        Codegen.generate("sge", Codegen.V0, Codegen.V0, Codegen.V1);

        Codegen.genPush(Codegen.V0);
    }

}

class AndNode extends LogicalExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" && ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {
        myExp1.codeGen();
        Codegen.genPop(Codegen.V0);
        Codegen.generate("li", Codegen.V1, "0");

        String isFalse = Codegen.nextLabel();
        Codegen.generate("beq", Codegen.V0, Codegen.V1, isFalse);

        myExp2.codeGen();
        String end = Codegen.nextLabel();
        Codegen.generate("j", end);

        Codegen.p.println(isFalse + ": ");
        Codegen.genPush(Codegen.V0);

        Codegen.p.println(end + ": ");
    }


}

class OrNode extends LogicalExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" || ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen() {


        myExp1.codeGen();
        Codegen.genPop(Codegen.V0);
        Codegen.generate("li", Codegen.V1, "1");

        String isTrue = Codegen.nextLabel();
        Codegen.generate("beq", Codegen.V0, Codegen.V1, isTrue);

        myExp2.codeGen();
        String end = Codegen.nextLabel();
        Codegen.generate("j", end);

        Codegen.p.println(isTrue + ": ");
        Codegen.genPush(Codegen.V0);

        Codegen.p.println(end + ": ");
    }
    
}