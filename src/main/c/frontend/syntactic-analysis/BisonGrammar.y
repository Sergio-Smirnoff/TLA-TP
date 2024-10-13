%{

#include "BisonActions.h"
#define YYDEBUG 1
yydebug = true;
%}

// You touch this, and you die.
%define api.value.union.name SemanticValue

%union {
	/** TerLOWERCASEals */

	char* string;
	Token token;

	/** Non-terLOWERCASEals */

	/*Function_body* function_body;*/
	Lexeme* lexeme;
	Regex_class* regex_class;
	Action* action;
	Range* range;
	Closure* closure;
	Java_function_body* java_function_body;
	Param* param;
	Rule* rule;
	Ruleset* ruleset;
	Program * program;
}


/**
 * Destructors. This FUNCTION_BODYs are executed after the parsing ends, so if the
 * AST must be used in the following phases of the compiler you shouldn't used
 * this approach. To use this mechanism, the AST must be translated into
 * another structure.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Destructor-Decl.html
 */

/**
%destructor { releaseFunctionBody($$); } <function_body>
%destructor { releaseClosure($$); } <closure>
%destructor { releaseParam($$); } <param>
%destructor { releaseRange($$); } <range>
%destructor { releaseRegexClass($$); } <regex_class>
%destructor { releaseAction($$); } <action>
%destructor { releaseLexeme($$); } <lexeme>
%destructor { releaseRule($$); } <rule>
%destructor { releaseRuleset($$); } <ruleset>
%destructor { releaseProgram($$); } <program>
*/

/** Terminals. */
%token <string> DIGIT
%token <string> LOWERCASE
%token <string> UPPERCASE
%token <string> SYMBOL
%token <string> ESCAPED_SYMBOL
%token <string> VAR_NAME
%token <string> STR
%token <string> DEFAULT

//%token <token> LOG
//%token <token> RETURN
%token <token> STAR
%token <token> PLUS
%token <token> MINUS
%token <token> DIV
%token <token> MOD
%token <token> COMMA
%token <token> BOOLEAN_TYPE
%token <token> STRING_TYPE
%token <token> INTEGER_TYPE
%token <token> DOUBLE_TYPE
%token <token> RANGER
%token <token> ENDLINE
%token <token> ARROW
%token <token> OPEN_BRACES
%token <token> CLOSE_BRACES
%token <token> OPEN_PARENTHESES
%token <token> CLOSE_PARENTHESES
%token <token> JAVA_RETURN
%token <token> JAVA_IF
%token <token> JAVA_ELSE
%token <token> JAVA_TRUE
%token <token> JAVA_FALSE
%token <token> JAVA_LEQ
%token <token> JAVA_GEQ
%token <token> JAVA_GREATER
%token <token> JAVA_LESSER
%token <token> JAVA_EXACT_COMPARISON
%token <token> JAVA_ASSIGNMENT
%token <token> JAVA_DOT_OPERATOR
%token <token> JAVA_DIAMOND
%token <token> JAVA_DOTS_OPERATOR
%token <token> JAVA_TERNARY_OPERATOR
%token <token> JAVA_OR
%token <token> JAVA_AND
%token <token> JAVA_FOR
%token <token> JAVA_WHILE
%token <token> JAVA_THROW
%token <token> JAVA_NOT
%token <token> JAVA_DIMS
%token <token> JAVA_NEW
%token <token> NUMBER
%token <token> FLOAT
%token <token> PIPE
%token <token> UNKNOWN





/** Non-terminals. */


%type <program> program
%type <lexeme> lexeme
%type <regex_class> regex_class
%type <action> action
%type <range> range
%type <param> param
%type <ruleset> ruleset
%type <rule> rule
%type <closure> closure

//%type <function_body> function_body

/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 * %left ADD SUB %left MUL DIV
 */


%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: ruleset													{ $$ = ProgramSemanticAction(currentCompilerState(), $1); }
	;

ruleset: rule ruleset												{ $$ = NULL; }
	| rule															{ $$ = NULL; }
	;

rule: VAR_NAME[def] regex_class[regex] ENDLINE[endline]	    		{ $$ = NULL; }
	| lexeme[lex] ARROW action[act] ENDLINE[endline]				{ $$ = NULL; }
	| lexeme[lex] ENDLINE[endline]									{ $$ = NULL; }
	;

lexeme: lexeme | lexeme												{ $$ = NULL; }
	| STR[string]													{ $$ = NULL; }					
	| OPEN_BRACES regex_class[regex] closure						{ $$ = NULL; }
	| OPEN_BRACES VAR_NAME[id] closure								{ $$ = NULL; }
	| DEFAULT[string]												{ $$ = NULL; }
	;

closure: CLOSE_BRACES STAR									 		{ $$ = NULL; }
	| CLOSE_BRACES PLUS										 		{ $$ = NULL; }					
	| CLOSE_BRACES													{ $$ = NULL; }	
	; 

regex_class: LOWERCASE												{ $$ = NULL; }
    | UPPERCASE														{ $$ = NULL; }
    | DIGIT															{ $$ = NULL; }
	| SYMBOL														{ $$ = NULL; }
	| ESCAPED_SYMBOL												{ $$ = NULL; }
    | range															{ $$ = NULL; }
	| LOWERCASE regex_class											{ $$ = NULL; }
    | UPPERCASE regex_class											{ $$ = NULL; }
    | DIGIT regex_class												{ $$ = NULL; }
    | range regex_class												{ $$ = NULL; }
	| SYMBOL regex_class											{ $$ = NULL; }
	| ESCAPED_SYMBOL regex_class									{ $$ = NULL; }
	;

range: LOWERCASE RANGER LOWERCASE									{ $$ = NULL; }
    | UPPERCASE RANGER UPPERCASE									{ $$ = NULL; }
    | DIGIT RANGER DIGIT											{ $$ = NULL; }
	| UPPERCASE RANGER LOWERCASE									{ $$ = NULL; }
	;

action: VAR_NAME													{ $$ = NULL; }
	| OPEN_PARENTHESES param Statement								{ $$ = NULL; }
	;

param: STRING_TYPE													{ $$ = NULL; }
    | INTEGER_TYPE													{ $$ = NULL; }
    | DOUBLE_TYPE													{ $$ = NULL; }
	| BOOLEAN_TYPE													{ $$ = NULL; }
	| %empty														{ $$ = NULL; }
	;




/* Abandon all hope ye who enter here
** Closely based on https://docs.oracle.com/javase/specs/jls/se17/jls17.pdf, an attempt at defining a syntactic grammar for Java
** This version is significantly reduced and simplified
**
** Note: Statement is the StatementNoShortIf following the model in 'The Java® Language Specification Java SE 17 Edition', no if statements without braces are accepted.
** Extending java parsing beyond this point far extends the scope of this program.
*/

NumericComparison: JAVA_GEQ
	| JAVA_GREATER
	| JAVA_LEQ
	| JAVA_LESSER
	;

Statement: StatementWithoutTrailingSubstatement
	| IfThenStatement
	| IfThenElseStatement
	| JAVA_WHILE OPEN_PARENTHESES Expression CLOSE_PARENTHESES OPEN_BRACES Statement CLOSE_BRACES
	| JAVA_FOR OPEN_PARENTHESES ForInit ENDLINE Expression ENDLINE StatementExpressionList CLOSE_PARENTHESES OPEN_BRACES Statement CLOSE_BRACES
	;

ForInit: StatementExpressionList
	| param VAR_NAME
	;

StatementExpressionList: %empty
	| StatementExpression
	| StatementExpression COMMA StatementExpressionList
	;

IfThenStatement: JAVA_IF OPEN_PARENTHESES Expression CLOSE_PARENTHESES Statement
	;

IfThenElseStatement: JAVA_IF OPEN_PARENTHESES Expression CLOSE_PARENTHESES Statement JAVA_ELSE Statement
	;

IfThenElseStatementNoShortIf: JAVA_IF OPEN_PARENTHESES Expression CLOSE_PARENTHESES Statement JAVA_ELSE Statement
	;

StatementWithoutTrailingSubstatement:
	| ENDLINE
	| StatementExpression
	| JAVA_RETURN Expression
	| JAVA_THROW Expression
	;

StatementExpression: Assignment
	| PLUS PLUS UnaryExpression
	| MINUS MINUS UnaryExpression
	| UnaryExpression PLUS PLUS 
	| UnaryExpression MINUS MINUS 
	| MethodInvocation
	;

VarAccess: VAR_NAME
	| VAR_NAME JAVA_DOT_OPERATOR VAR_NAME VarAccess
	| MethodInvocation
	;

MethodInvocation: VarAccess OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES
	;

ArgumentList: %empty
	| Expression
	| Expression COMMA ArgumentList
	;

Expression: ConditionalExpression
	| Assignment
	;

ConditionalExpression: ConditionalOrExpression
	| ConditionalOrExpression JAVA_TERNARY_OPERATOR Expression JAVA_DOTS_OPERATOR ConditionalExpression
	;

ConditionalOrExpression: ConditionalAndExpression
	| ConditionalOrExpression JAVA_OR ConditionalAndExpression
	;

ConditionalAndExpression: EqualityExpression
	| ConditionalAndExpression JAVA_AND EqualityExpression
	;

EqualityExpression: RelationalExpression
	| EqualityExpression JAVA_EXACT_COMPARISON RelationalExpression
	;

RelationalExpression: AdditiveExpression
	| RelationalExpression NumericComparison AdditiveExpression
	;

AdditiveExpression: MultiplicativeExpression
	| AdditiveExpression PLUS MultiplicativeExpression
	| AdditiveExpression MINUS MultiplicativeExpression
	;

MultiplicativeExpression: UnaryExpression
	| MultiplicativeExpression STAR UnaryExpression
	| MultiplicativeExpression DIV UnaryExpression
	| MultiplicativeExpression MOD UnaryExpression
	;

UnaryExpression: MINUS MINUS UnaryExpression
	| MINUS UnaryExpression
	| PLUS PLUS UnaryExpression
	| PLUS UnaryExpression
	| UnaryExpressionNotPlusMinus
	;

UnaryExpressionNotPlusMinus: PostfixExpression
	| JAVA_NOT UnaryExpression
	| OPEN_PARENTHESES param CLOSE_PARENTHESES
	| AdditiveExpression
	;

PostfixExpression:
	| Primary
	| VAR_NAME
	| PostfixExpression PLUS PLUS
	| PostfixExpression MINUS MINUS
	;

Assignment: VAR_NAME JAVA_ASSIGNMENT Expression
	;

Primary: PrimaryNoNewArray
	| ArrayCreationExpression
	;

PrimaryNoNewArray: Literal
	| OPEN_PARENTHESES Expression CLOSE_PARENTHESES
	| ClassInstanceCreationExpression
	| MethodInvocation
	;

ClassInstanceCreationExpression: UnqualifiedClassInstanceCreationExpression
	| VarAccess JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression
	| Primary JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression
	;

UnqualifiedClassInstanceCreationExpression: JAVA_NEW param OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES
	;

Literal: NUMBER
	| JAVA_TRUE
	| JAVA_FALSE
	| FLOAT
	| STR
	;

ArrayCreationExpression: JAVA_NEW param DimExprs Dims
	;

Dims: JAVA_DIMS
	| JAVA_DIMS JAVA_DIMS
	;

DimExprs: Expression
	| Expression Expression
	;

%%
