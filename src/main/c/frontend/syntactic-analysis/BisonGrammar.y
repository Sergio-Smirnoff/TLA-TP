%{

#include "BisonActions.h"
yydebug=1;

%}

// You touch this, and you die.
%define api.value.union.name SemanticValue

%union {
	/** Terminals */

	char* string;
	Token token;

	/** Non-terminals */

	Lexeme* lexeme;
	Lexeme_precursor* lexeme_precursor;
	Regex_class* regex_class;
	Action* action;
	Range* range;
	Closure* closure;
	Java_function_body* java_function_body;
	Param* param;
	Rule* rule;
	Ruleset* ruleset;
	Program * program;
	
	NumericComparison* NumericComparison;
	Block* Block;
	Statement* Statement;
	ForInit* ForInit;
	StatementExpressionList* StatementExpressionList;
	IfThenStatement* IfThenStatement;
	IfThenElseStatement* IfThenElseStatement;
	IfThenElseStatementNoShortIf* IfThenElseStatementNoShortIf;
	StatementWithoutTrailingSubstatement* StatementWithoutTrailingSubstatement;
	StatementExpression* StatementExpression;
	VarAccess* VarAccess;
	MethodInvocation* MethodInvocation;
	ArgumentList* ArgumentList;
	Expression* Expression;
	ConditionalExpression* ConditionalExpression;
	ConditionalOrExpression* ConditionalOrExpression;
	ConditionalAndExpression* ConditionalAndExpression;
	EqualityExpression* EqualityExpression;
	RelationalExpression* RelationalExpression;
	AdditiveExpression* AdditiveExpression;
	MultiplicativeExpression* MultiplicativeExpression;
	UnaryExpression* UnaryExpression;
	UnaryExpressionNotPlusMinus* UnaryExpressionNotPlusMinus;
	PostfixExpression* PostfixExpression;
	Assignment* Assignment;
	Primary* Primary;
	PrimaryNoNewArray* PrimaryNoNewArray;
	ClassInstanceCreationExpression* ClassInstanceCreationExpression;
	UnqualifiedClassInstanceCreationExpression* UnqualifiedClassInstanceCreationExpression;
	Literal* Literal;
	ArrayCreationExpression* ArrayCreationExpression;
	Dims* Dims;
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
%token <string> JAVA_FUNCTION_BODY

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
%type <lexeme_precursor> lexeme_precursor
%type <regex_class> regex_class
%type <action> action
%type <range> range
%type <param> param
%type <ruleset> ruleset
%type <rule> rule
%type <closure> closure

%type <NumericComparison> NumericComparison
%type <Block> Block
%type <Statement> Statement
%type <ForInit> ForInit
%type <StatementExpressionList> StatementExpressionList
%type <IfThenStatement> IfThenStatement
%type <IfThenElseStatement> IfThenElseStatement
%type <IfThenElseStatementNoShortIf> IfThenElseStatementNoShortIf
%type <StatementWithoutTrailingSubstatement> StatementWithoutTrailingSubstatement
%type <StatementExpression> StatementExpression
%type <VarAccess> VarAccess
%type <MethodInvocation> MethodInvocation
%type <ArgumentList> ArgumentList
%type <Expression> Expression
%type <ConditionalExpression> ConditionalExpression
%type <ConditionalOrExpression> ConditionalOrExpression
%type <ConditionalAndExpression> ConditionalAndExpression
%type <EqualityExpression> EqualityExpression
%type <RelationalExpression> RelationalExpression
%type <AdditiveExpression> AdditiveExpression
%type <MultiplicativeExpression> MultiplicativeExpression
%type <UnaryExpression> UnaryExpression
%type <UnaryExpressionNotPlusMinus> UnaryExpressionNotPlusMinus
%type <PostfixExpression> PostfixExpression
%type <Assignment> Assignment
%type <Primary> Primary
%type <PrimaryNoNewArray> PrimaryNoNewArray
%type <ClassInstanceCreationExpression> ClassInstanceCreationExpression
%type <UnqualifiedClassInstanceCreationExpression> UnqualifiedClassInstanceCreationExpression
%type <Literal> Literal
%type <ArrayCreationExpression> ArrayCreationExpression
%type <Dims> Dims

//%type <function_body> function_body

/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 * %left ADD SUB %left MUL DIV
 */
%left UPPERCASE LOWERCASE DIGIT SYMBOL ESCAPED_SYMBOL 

%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: ruleset													{ $$ = ProgramSemanticAction(currentCompilerState(), $1); }
	;

ruleset: rule ruleset												{$$ = RulesetSemanticAction( $1, $2); }
	| rule															{$$ = RulesetSemanticAction( $1, NULL); }
	;

rule: VAR_NAME[def] regex_class[regex] ENDLINE[endline]	    		{ $$ = RuleNewRegexSemanticAction($def, $regex, $endline); }
	| lexeme_precursor[lex] ARROW action ENDLINE[endline]					{ $$ = RuleDefinitionSemanticAction( $lex, $action, $endline, lexeme_action); }
	| lexeme_precursor[lex] ENDLINE[endline]						{ $$ = RuleDefinitionSemanticAction( $lex, NULL, $endline, ignore_lexeme ); }
	;

lexeme_precursor: lexeme lexeme_precursor							{ $$ = LexemePrecursorSemanticAction( $1, $2); }
	| lexeme														{ $$ = LexemePrecursorSemanticAction( $1, NULL); }
	;

lexeme: STR[string]													{ $$ = LexemeSemanticAction( $string, NULL, NULL, string); }					
	| regex_class[regex] closure									{ $$ = LexemeSemanticAction( NULL, $regex, $closure, regex_class); }
	| OPEN_BRACES VAR_NAME[id] CLOSE_BRACES closure					{ $$ = LexemeSemanticAction( $id, NULL, $closure, reg); }
	| DEFAULT[string]												{ $$ = LexemeSemanticAction( $string, NULL, NULL, def); }
	;

closure: %empty 													{ $$ = NULL; }
	| PLUS															{ $$ = ClosureSemanticAction($1); }
	| STAR															{ $$ = ClosureSemanticAction($1); }
	;

regex_class: LOWERCASE												{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | UPPERCASE														{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | DIGIT															{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| SYMBOL														{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| ESCAPED_SYMBOL												{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | range															{ $$ = RegexClassRangeSemanticAction($1, NULL); }
	| LOWERCASE regex_class											{ $$ = RegexClassStringSemanticAction($1, $2); }
    | UPPERCASE regex_class											{ $$ = RegexClassStringSemanticAction($1, $2); }
    | DIGIT regex_class												{ $$ = RegexClassStringSemanticAction($1, $2); }
    | range regex_class												{ $$ = RegexClassRangeSemanticAction($1, $2); }
	| SYMBOL regex_class											{ $$ = RegexClassStringSemanticAction($1, $2); }
	| ESCAPED_SYMBOL regex_class									{ $$ = RegexClassStringSemanticAction($1, $2); }
	;

range: LOWERCASE RANGER LOWERCASE									{ $$ = RangeSemanticAction($1, $3); }
    | UPPERCASE RANGER UPPERCASE									{ $$ = RangeSemanticAction($1, $3); }
    | DIGIT RANGER DIGIT											{ $$ = RangeSemanticAction($1, $3); }
	| UPPERCASE RANGER LOWERCASE									{ $$ = RangeSemanticAction($1, $3); }
	;

action: VAR_NAME													{ $$ = ActionSemanticAction($1); }
	| OPEN_PARENTHESES param CLOSE_PARENTHESES OPEN_BRACES Block CLOSE_BRACES								{ $$ = NULL; }
	;

param: STRING_TYPE													{ $$ = ParamSemanticAction($1); }
    | INTEGER_TYPE													{ $$ = ParamSemanticAction($1); }
    | DOUBLE_TYPE													{ $$ = ParamSemanticAction($1); }
	| BOOLEAN_TYPE													{ $$ = ParamSemanticAction($1); }
	| %empty														{ $$ = NULL; }
	;




/* Abandon all hope ye who enter here
** Closely based on https://docs.oracle.com/javase/specs/jls/se17/jls17.pdf, an attempt at defining a syntactic grammar for Java
** This version is significantly reduced and simplified
**
** Note: Statement is the StatementNoShortIf following the model in 'The Java® Language Specification Java SE 17 Edition', no if statements without braces are accepted.
** Extending java parsing beyond this point far extends the scope of this program.
*/

NumericComparison: JAVA_GEQ														{ $$ = NULL; }
	| JAVA_GREATER														{ $$ = NULL; }
	| JAVA_LEQ														{ $$ = NULL; }
	| JAVA_LESSER														{ $$ = NULL; }
	;

Block: Statement Block												{ $$ = NULL; }
	| Statement														{ $$ = NULL; }
	;

Statement: StatementWithoutTrailingSubstatement ENDLINE														{ $$ = NULL; }
	| IfThenStatement														{ $$ = NULL; }
	| IfThenElseStatement														{ $$ = NULL; }
	| JAVA_WHILE OPEN_PARENTHESES Expression CLOSE_PARENTHESES OPEN_BRACES Statement CLOSE_BRACES														{ $$ = NULL; }
	| JAVA_FOR OPEN_PARENTHESES ForInit ENDLINE Expression ENDLINE StatementExpressionList CLOSE_PARENTHESES OPEN_BRACES Statement CLOSE_BRACES														{ $$ = NULL; }
	;

ForInit: StatementExpressionList														{ $$ = NULL; }
	| param VAR_NAME														{ $$ = NULL; }
	;

StatementExpressionList: %empty														{ $$ = NULL; }
	| StatementExpression														{ $$ = NULL; }
	| StatementExpression COMMA StatementExpressionList														{ $$ = NULL; }
	;

IfThenStatement: JAVA_IF OPEN_PARENTHESES Expression CLOSE_PARENTHESES Statement														{ $$ = NULL; }
	;

IfThenElseStatement: JAVA_IF OPEN_PARENTHESES Expression CLOSE_PARENTHESES Statement JAVA_ELSE Statement														{ $$ = NULL; }
	;

IfThenElseStatementNoShortIf: JAVA_IF OPEN_PARENTHESES Expression CLOSE_PARENTHESES Statement JAVA_ELSE Statement														{ $$ = NULL; }
	;

StatementWithoutTrailingSubstatement: ENDLINE														{ $$ = NULL; }
	| StatementExpression														{ $$ = NULL; }
	| JAVA_RETURN Expression														{ $$ = NULL; }
	| JAVA_THROW Expression														{ $$ = NULL; }
	;

StatementExpression: Assignment														{ $$ = NULL; }
	| PLUS PLUS UnaryExpression														{ $$ = NULL; }
	| MINUS MINUS UnaryExpression														{ $$ = NULL; }
	| UnaryExpression PLUS PLUS 														{ $$ = NULL; }
	| UnaryExpression MINUS MINUS														{ $$ = NULL; } 
	| MethodInvocation														{ $$ = NULL; }
	| param VAR_NAME JAVA_ASSIGNMENT Expression								{ $$ = NULL; }
	;

VarAccess: VAR_NAME														{ $$ = NULL; }
	| VAR_NAME JAVA_DOT_OPERATOR VarAccess														{ $$ = NULL; }
	| param JAVA_DOT_OPERATOR VarAccess										{ $$ = NULL; }
	| MethodInvocation														{ $$ = NULL; }
	;

MethodInvocation: VarAccess OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES														{ $$ = NULL; }
	;

ArgumentList: %empty														{ $$ = NULL; }
	| Expression														{ $$ = NULL; }
	| Expression COMMA ArgumentList														{ $$ = NULL; }
	;

Expression: ConditionalExpression														{ $$ = NULL; }
	| Assignment														{ $$ = NULL; }
	;

ConditionalExpression: ConditionalOrExpression														{ $$ = NULL; }
	| ConditionalOrExpression JAVA_TERNARY_OPERATOR Expression JAVA_DOTS_OPERATOR ConditionalExpression														{ $$ = NULL; }
	;

ConditionalOrExpression: ConditionalAndExpression														{ $$ = NULL; }
	| ConditionalOrExpression JAVA_OR ConditionalAndExpression														{ $$ = NULL; }
	;

ConditionalAndExpression: EqualityExpression														{ $$ = NULL; }
	| ConditionalAndExpression JAVA_AND EqualityExpression														{ $$ = NULL; }
	;

EqualityExpression: RelationalExpression														{ $$ = NULL; }
	| EqualityExpression JAVA_EXACT_COMPARISON RelationalExpression														{ $$ = NULL; }
	;

RelationalExpression: AdditiveExpression														{ $$ = NULL; }
	| RelationalExpression NumericComparison AdditiveExpression														{ $$ = NULL; }
	;

AdditiveExpression: MultiplicativeExpression														{ $$ = NULL; }
	| AdditiveExpression PLUS MultiplicativeExpression														{ $$ = NULL; }
	| AdditiveExpression MINUS MultiplicativeExpression														{ $$ = NULL; }
	;

MultiplicativeExpression: UnaryExpression														{ $$ = NULL; }
	| MultiplicativeExpression STAR UnaryExpression														{ $$ = NULL; }
	| MultiplicativeExpression DIV UnaryExpression														{ $$ = NULL; }
	| MultiplicativeExpression MOD UnaryExpression														{ $$ = NULL; }
	;

UnaryExpression: MINUS MINUS UnaryExpression														{ $$ = NULL; }
	| MINUS UnaryExpression														{ $$ = NULL; }
	| PLUS PLUS UnaryExpression														{ $$ = NULL; }
	| PLUS UnaryExpression														{ $$ = NULL; }
	| UnaryExpressionNotPlusMinus														{ $$ = NULL; }
	;

UnaryExpressionNotPlusMinus: PostfixExpression														{ $$ = NULL; }
	| JAVA_NOT UnaryExpression														{ $$ = NULL; }
	| OPEN_PARENTHESES param CLOSE_PARENTHESES														{ $$ = NULL; }
	| AdditiveExpression														{ $$ = NULL; }
	;

PostfixExpression: Primary														{ $$ = NULL; }
	| VarAccess														{ $$ = NULL; }
	| PostfixExpression PLUS PLUS														{ $$ = NULL; }
	| PostfixExpression MINUS MINUS														{ $$ = NULL; }
	;

Assignment: VarAccess JAVA_ASSIGNMENT Expression														{ $$ = NULL; }
	;

Primary: PrimaryNoNewArray														{ $$ = NULL; }
	| ArrayCreationExpression														{ $$ = NULL; }
	;

PrimaryNoNewArray: Literal														{ $$ = NULL; }
	| OPEN_PARENTHESES Expression CLOSE_PARENTHESES														{ $$ = NULL; }
	| ClassInstanceCreationExpression														{ $$ = NULL; }
	| MethodInvocation														{ $$ = NULL; }
	;

ClassInstanceCreationExpression: UnqualifiedClassInstanceCreationExpression														{ $$ = NULL; }
	| VarAccess JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression														{ $$ = NULL; }
	| Primary JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression														{ $$ = NULL; }
	;

UnqualifiedClassInstanceCreationExpression: JAVA_NEW param OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES														{ $$ = NULL; }
	;

Literal: NUMBER														{ $$ = NULL; }
	| JAVA_TRUE														{ $$ = NULL; }
	| JAVA_FALSE														{ $$ = NULL; }
	| FLOAT														{ $$ = NULL; }
	| STR														{ $$ = NULL; }
	;

ArrayCreationExpression: JAVA_NEW param Dims														{ $$ = NULL; }
	;

Dims: JAVA_DIMS														{ $$ = NULL; }
	| JAVA_DIMS Dims														{ $$ = NULL; }
	;

%%
