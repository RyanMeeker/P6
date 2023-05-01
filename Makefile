###
#
# make clean removes all generated files.
#
###

JC = javac

CP = ./deps:.

P6.class: P6.java parser.class Yylex.class ASTnode.class
	$(JC) -g -cp $(CP) P6.java

parser.class: parser.java ASTnode.class Yylex.class ErrMsg.class
	$(JC) -g -cp $(CP) parser.java

parser.java: brevis.cup
	java -cp $(CP) java_cup.Main < brevis.cup

Yylex.class: brevis.jlex.java sym.class ErrMsg.class
	$(JC) -g -cp $(CP) brevis.jlex.java

ASTnode.class: ast.java Type.java Sym.class Codegen.java
	$(JC) -g -cp $(CP) ast.java Type.java Codegen.java

brevis.jlex.java: brevis.jlex sym.class
	java -cp $(CP) JLex.Main brevis.jlex

sym.class: sym.java
	$(JC) -g -cp $(CP) sym.java

sym.java: brevis.cup
	java -cp $(CP) java_cup.Main < brevis.cup

ErrMsg.class: ErrMsg.java
	$(JC) -g -cp $(CP) ErrMsg.java

Sym.class: Sym.java Type.class ast.java
	$(JC) -g -cp $(CP) Sym.java ast.java

SymTab.class: SymTab.java Sym.class SymDuplicationException.class SymTabEmptyException.class
	$(JC) -g -cp $(CP) SymTab.java

Type.class: Type.java ast.java Sym.java
	$(JC) -g -cp $(CP) Type.java ast.java Sym.java

Codegen.class: Codegen.java
	$(JC) -g -cp $(CP) Codegen.java

SymDuplicationException.class: SymDuplicationException.java
	$(JC) -g -cp $(CP) SymDuplicationException.java

SymTabEmptyException.class: SymTabEmptyException.java
	$(JC) -g -cp $(CP) SymTabEmptyException.java

###
# test
###
test:
	java -cp $(CP) P6 test.brevis test.s

###
# clean
###
clean:
	rm -f *~ *.class parser.java brevis.jlex.java sym.java

cleantest:
	rm -f *.s
