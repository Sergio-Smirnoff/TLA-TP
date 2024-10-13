%{

#include "BisonActions.h"
//yydebug=1;

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
	Regexes *regexes;
	Symbols *symbols;
	Symbol *symbol;
	Action* action;
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
	UnaryExpression* UnaryExpression;
	PostfixExpression* PostfixExpression;
	Assignment* Assignment;
	Primary* Primary;
	ClassInstanceCreationExpression* ClassInstanceCreationExpression;
	UnqualifiedClassInstanceCreationExpression* UnqualifiedClassInstanceCreationExpression;
	Literal* Literal;
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
%token <token> INCREMENT
%token <token> DECREMENT
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
%token <token> OPEN_BRACKET
%token <token> CLOSE_BRACKET

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
%token <token> JAVA_NEW
%token <token> NUMBER
%token <token> FLOAT
%token <token> UNKNOWN

/** Non-terminals. */


%type <program> program
%type <lexeme> lexeme
%type <lexeme_precursor> lexeme_precursor
%type <regex_class> regex_class
%type <symbol> symbol
%type <symbols> symbols
%type <regexes> regexes
%type <action> action
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
%type <UnaryExpression> UnaryExpression
%type <PostfixExpression> PostfixExpression
%type <Assignment> Assignment
%type <Primary> Primary
%type <ClassInstanceCreationExpression> ClassInstanceCreationExpression
%type <UnqualifiedClassInstanceCreationExpression> UnqualifiedClassInstanceCreationExpression
%type <Literal> Literal

//%type <function_body> function_body

/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 * %left ADD SUB %left MUL DIV
 */

%left ','
%precedence INCREMENT DECREMENT CLOSE_PARENTHESES
%left JAVA_DOT_OPERATOR JAVA_DOTS_OPERATOR
%left PLUS MINUS              
%left STAR DIV MOD           
%left JAVA_OR                
%left JAVA_AND               
%nonassoc JAVA_TERNARY_OPERATOR 
%left JAVA_LESSER JAVA_LEQ JAVA_GREATER JAVA_GEQ JAVA_EXACT_COMPARISON JAVA_ASSIGNMENT   
%right JAVA_NOT              
%left UPPERCASE LOWERCASE DIGIT SYMBOL ESCAPED_SYMBOL
%left  OPEN_PARENTHESES
%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: ruleset																																		{ $$ = ProgramSemanticAction(currentCompilerState(), $1); }
	;

ruleset: rule ruleset																																	{ $$ = RulesetSemanticAction( $1, $2); }
	| rule																																				{ $$ = RulesetSemanticAction( $1, NULL); }
	;

rule: VAR_NAME[def] OPEN_BRACKET regexes[regex] CLOSE_BRACKET ENDLINE[endline]	    		{ $$ = RuleNewRegexSemanticAction($def, $regex, $endline); }
	| lexeme_precursor[lex] ARROW action ENDLINE[endline]					{ $$ = RuleDefinitionSemanticAction( $lex, $action, $endline, lexeme_action); }
	| lexeme_precursor[lex] ENDLINE[endline]						{ $$ = RuleDefinitionSemanticAction( $lex, NULL, $endline, ignore_lexeme ); }
	;

lexeme_precursor: lexeme lexeme_precursor							{ $$ = LexemePrecursorSemanticAction( $1, $2); }
	| lexeme														{ $$ = LexemePrecursorSemanticAction( $1, NULL); }
	| DEFAULT[string]												{ $$ = LexemeSemanticAction( $string, NULL, NULL, def); }
	| STR[string]													{ $$ = LexemeSemanticAction( $string, NULL, NULL, string); }
	;

lexeme: OPEN_BRACKET regex_class[regex] CLOSE_BRACKET closure									{ $$ = LexemeSemanticAction( NULL, $regex, $closure, regex_class); }
	| OPEN_BRACES VAR_NAME[id] CLOSE_BRACES closure					{ $$ = LexemeSemanticAction( $id, NULL, $closure, reg); }
	;

closure: %empty 																																		{ $$ = NULL; }
	| PLUS																																				{ $$ = ClosureSemanticAction($1); }
	| STAR																																				{ $$ = ClosureSemanticAction($1); }
	;

regexes: regex_class { $$ = NULL; }
	| regex_class regexes { $$ = NULL; }
	;

regex_class: symbols												{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | symbol RANGER symbol															{ $$ = RangeSemanticAction($1, $3); }
	;

symbols: symbol { $$ = NULL; }
	| symbol symbols { $$ = NULL; }
	;

symbol: LOWERCASE 																																		{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| UPPERCASE 																																		{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| DIGIT 																																			{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| SYMBOL 																																			{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| ESCAPED_SYMBOL 																																	{ $$ = RegexClassStringSemanticAction($1, NULL); }
	
	;

action: VAR_NAME																																		{ $$ = ActionSemanticAction($1); }
	| OPEN_PARENTHESES param[param] CLOSE_PARENTHESES OPEN_BRACES Block[block] CLOSE_BRACES																{ $$ = ActionJavaSemanticAction($param, $block); }
	;

param: STRING_TYPE																																		{ $$ = ParamSemanticAction($1); }
    | INTEGER_TYPE																																		{ $$ = ParamSemanticAction($1); }
    | DOUBLE_TYPE																																		{ $$ = ParamSemanticAction($1); }
	| BOOLEAN_TYPE																																		{ $$ = ParamSemanticAction($1); }
	| %empty																																			{ $$ = NULL; }
	;




/* Abandon all hope ye who enter here
** Closely based on https://docs.oracle.com/javase/specs/jls/se17/jls17.pdf, an attempt at defining a syntactic grammar for Java
** This version is significantly reduced and simplified
**
** Note: Statement is the StatementNoShortIf following the model in 'The Java® Language Specification Java SE 17 Edition', no if statements without braces are accepted.
** Extending java parsing beyond this point far extends the scope of this program.
*/

NumericComparison: JAVA_GEQ																																{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_GREATER																																		{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_LEQ																																			{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_LESSER																																		{ $$ = JavaNumericComparisonSemanticAction($1); }
	;

Block: Statement Block																																	{ $$ = JavaBlockSemanticAction($1, $2); }
	| Statement																																			{ $$ = JavaBlockSemanticAction($1, NULL); }
	;

Statement: StatementWithoutTrailingSubstatement ENDLINE																									{ $$ = InlineStatementSemanticAction($1); }
	| IfThenStatement																																	{ $$ = IfStatementSemanticAction($1); }
	| IfThenElseStatement																																{ $$ = IfElseStatement($1); }
	| JAVA_WHILE OPEN_PARENTHESES Expression[exp] CLOSE_PARENTHESES OPEN_BRACES Statement[state] CLOSE_BRACES											{ $$ = WhileStatementSemanticAction($exp, $state); }
	| JAVA_FOR OPEN_PARENTHESES ForInit[init] ENDLINE Expression[exp] ENDLINE StatementExpressionList[stlist] CLOSE_PARENTHESES OPEN_BRACES Statement[state] CLOSE_BRACES			{ $$ = ForStatementSemanticAction($init, $exp, $stlist, $state); }
	;

ForInit: StatementExpressionList																														{ $$ = JavaStatementExpressionListSemanticAction($1); }
	| param VAR_NAME																																	{ $$ = JavaVarTypeDefinitionSemantictAction($1, $2); }
	;

StatementExpressionList: %empty																															{ $$ = NULL; }
	| StatementExpression																																{ $$ = StatementExpressionListSemanticAction($1, NULL); }
	| StatementExpression COMMA StatementExpressionList																									{ $$ = StatementExpressionListSemanticAction($1, $2); }
	;

IfThenStatement: JAVA_IF OPEN_PARENTHESES Expression[expression] CLOSE_PARENTHESES Statement[ifstatement]												{ $$ = JavaIfThenStructureSemanticAction($expression, $ifstatement, NULL); }
	;

IfThenElseStatement: JAVA_IF OPEN_PARENTHESES Expression[expression] CLOSE_PARENTHESES Statement[ifstatement] JAVA_ELSE Statement[elsestatement]		{ $$ = JavaIfThenStructureSemanticAction($expression, $ifstatement, $elsestatement); }
	;

StatementWithoutTrailingSubstatement: %empty																											{ $$ = NULL; }
	| StatementExpression																																{ $$ = JavaStatementExpressionSemanticAction($1); }
	| JAVA_RETURN Expression																															{ $$ = JavaReturnExpressionSemanticAction($1); }
	| JAVA_THROW Expression																																{ $$ = JavaThrowExpressionSemanticAction($1); }
	;

StatementExpression: Assignment																															{ $$ = JavaAsignmentSemanticAction($1); }
	| INCREMENT UnaryExpression																															{ $$ = JavaModifyingStatementExpressionSemanticAction(pre, $1, $2); }
	| DECREMENT UnaryExpression																											     			{ $$ = JavaModifyingStatementExpressionSemanticAction(pre, $1, $2); }
	| UnaryExpression INCREMENT 																														{ $$ = JavaModifyingStatementExpressionSemanticAction(post, $2, $1); }
	| UnaryExpression DECREMENT																															{ $$ = JavaModifyingStatementExpressionSemanticAction(post, $2, $1); } 
	| MethodInvocation																																	{ $$ = JavaMethodInvocationSemanticAction($1); }
	| param VAR_NAME JAVA_ASSIGNMENT Expression																											{ $$ = JavaAsignmentParamSemanticAction($1, $2, $3, $4); }
	;

VarAccess: VAR_NAME																																		{ $$ = VarAccessVarSemanticAction($1); }
	| VAR_NAME JAVA_DOT_OPERATOR VarAccess																												{ $$ = VarAccessVarOperatorSemanticAction($1,$3); }
	| param JAVA_DOT_OPERATOR VarAccess																													{ $$ = VarAccessParamOperatorSemanticAction($1,$3); }
	| MethodInvocation																																	{ $$ = VarAccessMethodInvocationSemanticAction($1); }
	;

MethodInvocation: VarAccess OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES																				{ $$ = InvocationSemanticAction($1, $2 ); }
	;

ArgumentList: %empty																																	{ $$ = NULL; }
	| Expression																																		{ $$ = ArgListSemanticExpression($1,NULL); }
	| Expression COMMA ArgumentList																														{ $$ = ArgListSemanticExpression($1,$2); }
	;

Expression: ConditionalExpression																														{ $$ = expressionSematicAction( $1, NULL, cexp ); }
	| Assignment																																		{ $$ = expressionSematicAction( NULL, $1, assignment ); }
	;

ConditionalExpression: ConditionalOrExpression																											{ $$ = JavaConditionalExpSemanticAction($1,NULL,NULL); }
	| ConditionalOrExpression JAVA_TERNARY_OPERATOR Expression JAVA_DOTS_OPERATOR ConditionalExpression													{ $$ = JavaConditionalExpSemanticAction($1, $3, $5); }
	;

ConditionalOrExpression: ConditionalAndExpression																										{ $$ = JavaConditionalOrExpressionSemanticAction( $1 ); }
	| ConditionalOrExpression JAVA_OR ConditionalAndExpression																							{ $$ = JavaConditionalOrExpressionSemanticAction( $3, $1 ); }
	;

ConditionalAndExpression: EqualityExpression																											{ $$ = JavaConditionalAndExpressionSemanticAction($1); }
	;

EqualityExpression: UnaryExpression																														{ $$ = EqualityExpressionSemanticAction( $1 ); }
	| EqualityExpression JAVA_EXACT_COMPARISON UnaryExpression																							{ $$ = EqualityExpressionSemanticAction( $2,$1 ); }
	;

UnaryExpression:  UnaryExpression NumericComparison UnaryExpression																						{ $$ = UnaryExpressionNumericComparisonSintaticAction($1,$2;$3); }
	| UnaryExpression STAR UnaryExpression																												{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,star ,$3); }
	| UnaryExpression DIV UnaryExpression																												{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,div ,$3); }
	| UnaryExpression MOD UnaryExpression																												{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,mod ,$3); }
	| UnaryExpression PLUS UnaryExpression																												{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,plus ,$3); }
	| UnaryExpression MINUS UnaryExpression																												{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,minus ,$3); }
	| PostfixExpression																																	{ $$ = UnaryExpressionPostfixExpressionSintaticAction($1); }
	| JAVA_NOT UnaryExpression																															{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| OPEN_PARENTHESES param CLOSE_PARENTHESES																											{ $$ = UnaryExpressionParamSintaticAction($2); }
	| DECREMENT UnaryExpression																															{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| MINUS UnaryExpression																																{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| INCREMENT UnaryExpression																															{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| PLUS UnaryExpression																																{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	;


PostfixExpression: Primary																																{ $$ = PostfixExpressionPrimarySemanticAction($1); }
	| VarAccess																																			{ $$ = PostfixExpressionVAccessSemanticAction($1); }
	| VarAccess INCREMENT																																{ $$ = PostfixExpressionVAccessSemanticAction($1,$2); }
	| VarAccess DECREMENT																																{ $$ = PostfixExpressionVAccessSemanticAction($1, $2); }
	;

Assignment: VarAccess JAVA_ASSIGNMENT Expression																										{ $$ = AssignmentSemanticAction($1,$3); }
	;

Primary: Literal																																		{ $$ = PrimaryLiteralSemanticAction($1); }
	| OPEN_PARENTHESES Expression CLOSE_PARENTHESES																										{ $$ = PrimaryExpressionSemanticAction($2); }
	| ClassInstanceCreationExpression																													{ $$ = PrimaryCExpSemanticAction($1); }
	;

ClassInstanceCreationExpression: UnqualifiedClassInstanceCreationExpression																				{ $$ = InstanceCreationExpressionSemanticAction($1); }
	| VarAccess JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression																			{ $$ = VAccessInstanceCreationExpressionSemanticAction($1, $3); }
	| Primary JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression																				{ $$ = PrimaryInstanceCreationExpressionSemanticAction($1,$3); }
	;

UnqualifiedClassInstanceCreationExpression: JAVA_NEW param OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES												{ $$ = UnqualifiedClassSemanticAction($2,$4); }
	;

Literal: NUMBER																																			{ $$ = JavaLiteralTokenSemanticAction($1); }
	| JAVA_TRUE																																			{ $$ = JavaLiteralTokenSemanticAction($1); }
	| JAVA_FALSE																																		{ $$ = JavaLiteralTokenSemanticAction($1); }
	| FLOAT																																				{ $$ = JavaLiteralTokenSemanticAction($1); }
	| STR																																				{ $$ = JavaLiteralStrSemanticAction($1); }
	;

%%
