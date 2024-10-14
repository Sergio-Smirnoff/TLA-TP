%{

#include "BisonActions.h"

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
	Symbol *symbol;
	Action* action;
	Closure* closure;
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

/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 * %left ADD SUB %left MUL DIV
 */

%precedence INCREMENT DECREMENT CLOSE_PARENTHESES
%nonassoc VAR_NAME
%left JAVA_DOT_OPERATOR
%left JAVA_DOTS_OPERATOR
%left PLUS 
%left MINUS              
%left STAR
%left DIV
%left MOD           
%left JAVA_OR                
%left JAVA_AND               
%nonassoc JAVA_TERNARY_OPERATOR 
%left JAVA_LESSER 
%left JAVA_LEQ
%left JAVA_GREATER
%left JAVA_GEQ
%left JAVA_EXACT_COMPARISON
%left JAVA_ASSIGNMENT   
%right JAVA_NOT              
%left UPPERCASE LOWERCASE DIGIT SYMBOL ESCAPED_SYMBOL
%left  OPEN_PARENTHESES
%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: ruleset																																									{ $$ = ProgramSemanticAction(currentCompilerState(), $1); }
	;

ruleset: rule ruleset																																								{ $$ = RulesetSemanticAction($1, $2); }
	| rule																																											{ $$ = RulesetSemanticAction($1, NULL); }
	;

rule: VAR_NAME[def] OPEN_BRACKET regexes[regex] CLOSE_BRACKET ENDLINE	    																										{ $$ = RuleNewRegexSemanticAction($def, $regex); }
	| lexeme_precursor[lex] ARROW action ENDLINE																																	{ $$ = RuleDefinitionSemanticAction($lex, $action, lexeme_action); }
	| lexeme_precursor[lex] ENDLINE																																					{ $$ = RuleDefinitionSemanticAction($lex, NULL, ignore_lexeme); }
	;

lexeme_precursor: lexeme lexeme_precursor																																			{ $$ = LexemePrecursorSemanticAction($1, $2); }
	| lexeme																																										{ $$ = LexemePrecursorSemanticAction($1, NULL); }
	| DEFAULT[string]																																								{ $$ = LexemeStringSemanticAction($string, default_lexeme); }
	| STR[string]																																									{ $$ = LexemeStringSemanticAction($string, string_lexeme); }
	;

lexeme: OPEN_BRACKET regexes[regex] CLOSE_BRACKET closure[closure_p]																												{ $$ = LexemeSemanticAction(NULL, $regex, $closure_p, regexes); }
	| OPEN_BRACES VAR_NAME[id] CLOSE_BRACES closure[closure_p]																														{ $$ = LexemeSemanticAction($id, NULL, $closure_p, name); }
	;

closure: %empty 																																									{ $$ = NULL; }
	| PLUS																																											{ $$ = ClosureSemanticAction($1); }
	| STAR																																											{ $$ = ClosureSemanticAction($1); }
	;

regexes: regex_class 																																								{ $$ = RegexesSemanticAction($1, NULL); }
	| regex_class regexes 																																							{ $$ = RegexesSemanticAction($1, $2); }
	;

regex_class: symbol																																									{ $$ = SymbolRegexSemanticAction($1); }
    | symbol RANGER symbol																																							{ $$ = RegexClassRangeSemanticAction($1, $3); }
	| OPEN_BRACES VAR_NAME[id] CLOSE_BRACES closure[clousure]																														{ $$ = CreatedClassSemanticAction($id, $clousure); }
	;

symbol: LOWERCASE 																																									{ $$ = RegexSymbolSemanticAction($1); }
	| UPPERCASE 																																									{ $$ = RegexSymbolSemanticAction($1); }
	| DIGIT 																																										{ $$ = RegexSymbolSemanticAction($1); }
	| SYMBOL 																																										{ $$ = RegexSymbolSemanticAction($1); }
	| ESCAPED_SYMBOL 																																								{ $$ = RegexSymbolSemanticAction($1); }
	;

action: VAR_NAME																																									{ $$ = ActionSemanticAction($1); }
	| OPEN_PARENTHESES param[param_p] CLOSE_PARENTHESES OPEN_BRACES Block[block] CLOSE_BRACES																							{ $$ = ActionJavaSemanticAction($param_p, $block); }
	| OPEN_PARENTHESES CLOSE_PARENTHESES OPEN_BRACES Block[block] CLOSE_BRACES																										{ $$ = ActionJavaSemanticAction(NULL, $block); }
	;

param: STRING_TYPE																																									{ $$ = ParamSemanticAction($1); }
    | INTEGER_TYPE																																									{ $$ = ParamSemanticAction($1); }
    | DOUBLE_TYPE																																									{ $$ = ParamSemanticAction($1); }
	| BOOLEAN_TYPE																																									{ $$ = ParamSemanticAction($1); }
	;


/* Abandon all hope ye who enter here
** Closely based on https://docs.oracle.com/javase/specs/jls/se17/jls17.pdf, an attempt at defining a syntactic grammar for Java
** This version is significantly reduced and simplified
**
** Note: Statement is the StatementNoShortIf following the model in 'The Java® Language Specification Java SE 17 Edition', no if statements without braces are accepted.
** Extending java parsing beyond this point far extends the scope of this program.
*/

NumericComparison: JAVA_GEQ																																							{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_GREATER																																									{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_LEQ																																										{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_LESSER																																									{ $$ = JavaNumericComparisonSemanticAction($1); }
	;

Block: Statement Block																																								{ $$ = JavaBlockSemanticAction($1, $2); }
	| Statement																																										{ $$ = JavaBlockSemanticAction($1, NULL); }
	| JAVA_RETURN Expression ENDLINE																																				{ $$ = JavaReturnExpressionSemanticAction($2); }
	| JAVA_THROW Expression ENDLINE																																					{ $$ = JavaThrowExpressionSemanticAction($2); }
	;

Statement: ENDLINE																																									{ $$ = NULL; }
	| StatementExpression																																							{ $$ = JavaStatementExpressionSemanticAction($1); }
	| IfThenStatement																																								{ $$ = IfStatementSemanticAction($1); }
	| JAVA_WHILE OPEN_PARENTHESES Expression[exp] CLOSE_PARENTHESES OPEN_BRACES Statement[state] CLOSE_BRACES																		{ $$ = WhileStatementSemanticAction($exp, $state); }
	| JAVA_FOR OPEN_PARENTHESES ForInit[init] ENDLINE Expression[exp] ENDLINE StatementExpressionList[stlist] CLOSE_PARENTHESES OPEN_BRACES Statement[state] CLOSE_BRACES			{ $$ = ForStatementSemanticAction($init, $exp, $stlist, $state); }
	;

ForInit: StatementExpressionList																																					{ $$ = ForInitExpressionListSemanticAction($1); }
	| param VAR_NAME																																								{ $$ = JavaVarTypeDefinitionSemantictAction($1, $2, withParams); }
	| VAR_NAME																																										{ $$ = JavaVarTypeDefinitionSemantictAction(NULL, $1, withoutParams); }
	;

StatementExpressionList: %empty																																						{ $$ = NULL; }
	| StatementExpression																																							{ $$ = StatementExpressionListSemanticAction($1, NULL); }
	| StatementExpression COMMA StatementExpressionList																																{ $$ = StatementExpressionListSemanticAction($1, $3); }
	;

IfThenStatement: JAVA_IF OPEN_PARENTHESES Expression[expression] CLOSE_PARENTHESES OPEN_BRACES Statement[ifstatement] CLOSE_BRACES																			{ $$ = JavaIfThenStructureSemanticAction($expression, $ifstatement, NULL); }
	| JAVA_IF OPEN_PARENTHESES Expression[expression] CLOSE_PARENTHESES OPEN_BRACES Statement[ifstatement] CLOSE_BRACES JAVA_ELSE OPEN_BRACES Statement[elsestatement] CLOSE_BRACES													{ $$ = JavaIfThenStructureSemanticAction($expression, $ifstatement, $elsestatement); }
	;

StatementExpression: Assignment																																						{ $$ = JavaAsignmentSemanticAction($1); }
	| MethodInvocation																																								{ $$ = JavaMethodInvocationSemanticAction($1); }
	| param VAR_NAME JAVA_ASSIGNMENT Expression																																		{ $$ = JavaAsignmentParamSemanticAction($1, $2, $3, $4); }
	;

VarAccess: VAR_NAME																																									{ $$ = VarAccessVarSemanticAction($1); }
	| VAR_NAME JAVA_DOT_OPERATOR VarAccess																																			{ $$ = VarAccessVarOperatorSemanticAction($1,$3); }
	| param JAVA_DOT_OPERATOR VarAccess																																				{ $$ = VarAccessParamOperatorSemanticAction($1,$3); }
	| MethodInvocation																																								{ $$ = VarAccessMethodInvocationSemanticAction($1); }
	;

MethodInvocation: VarAccess OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES																											{ $$ = InvocationSemanticAction($1, $3 ); }
	;

ArgumentList: %empty																																								{ $$ = NULL; }
	| Expression																																									{ $$ = ArgListSemanticExpression($1,NULL); }
	| Expression COMMA ArgumentList																																					{ $$ = ArgListSemanticExpression($1,$3); }
	;

Expression: ConditionalExpression																																					{ $$ = expressionSematicAction($1, NULL); }
	| Assignment																																									{ $$ = expressionSematicAction(NULL, $1); }
	;

ConditionalExpression: ConditionalOrExpression																																		{ $$ = JavaConditionalExpSemanticAction($1,NULL,NULL); }
	| ConditionalOrExpression JAVA_TERNARY_OPERATOR Expression JAVA_DOTS_OPERATOR ConditionalExpression																				{ $$ = JavaConditionalExpSemanticAction($1, $3, $5); }
	;

ConditionalOrExpression: ConditionalAndExpression																																	{ $$ = JavaConditionalOrExpressionSemanticAction($1, NULL); }
	| ConditionalOrExpression JAVA_OR ConditionalAndExpression																														{ $$ = JavaConditionalOrExpressionSemanticAction($3, $1 ); }
	;

ConditionalAndExpression: EqualityExpression																																		{ $$ = JavaConditionalAndExpressionSemanticAction(NULL, $1); }
	| ConditionalAndExpression JAVA_AND EqualityExpression                                                        																	{ $$ = JavaConditionalAndExpressionSemanticAction($1,$3); }
	;

EqualityExpression: UnaryExpression												{ $$ = EqualityExpressionSemanticAction($1, NULL); }
	| EqualityExpression JAVA_EXACT_COMPARISON UnaryExpression																														{ $$ = EqualityExpressionSemanticAction($3,$1); }
	;

UnaryExpression:  UnaryExpression NumericComparison PostfixExpression																													{ $$ = UnaryExpressionNumericComparisonSintaticAction($1,$2,$3); }
	| UnaryExpression STAR PostfixExpression																																		{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,star_t ,$3); }
	| UnaryExpression DIV PostfixExpression																																		{ $$ = UnaryExpressionDoubleTokenSintaticAction($1, div_type,$3); }
	| UnaryExpression MOD PostfixExpression																																		{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,mod_t ,$3); }
	| UnaryExpression PLUS PostfixExpression																																		{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,plus_t ,$3); }
	| UnaryExpression MINUS PostfixExpression																																		{ $$ = UnaryExpressionDoubleTokenSintaticAction($1,minus_t ,$3); }
	| PostfixExpression																																								{ $$ = UnaryExpressionPostfixExpressionSintaticAction($1); }
	| JAVA_NOT UnaryExpression																																						{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| OPEN_PARENTHESES param CLOSE_PARENTHESES																																		{ $$ = UnaryExpressionParamSintaticAction($2); }
	| OPEN_PARENTHESES CLOSE_PARENTHESES																																			{ $$ = UnaryExpressionParamSintaticAction(NULL); }
	| DECREMENT UnaryExpression																																						{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| MINUS UnaryExpression																																							{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| INCREMENT UnaryExpression																																						{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| PLUS UnaryExpression																																							{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	;


PostfixExpression: Primary																																							{ $$ = PostfixExpressionPrimarySemanticAction($1); }
	| VarAccess																																										{ $$ = PostfixExpressionVAccessDefaultSemanticAction($1); }
	| VarAccess INCREMENT																																							{ $$ = PostfixExpressionVAccessSemanticAction($1,$2); }
	| VarAccess DECREMENT																																							{ $$ = PostfixExpressionVAccessSemanticAction($1, $2); }
	;

Assignment: VarAccess JAVA_ASSIGNMENT Expression																																	{ $$ = AssignmentSemanticAction($1,$3); }
	;

Primary: Literal																							{ $$ = PrimaryLiteralSemanticAction($1); }
	| OPEN_PARENTHESES Expression CLOSE_PARENTHESES															{ $$ = PrimaryExpressionSemanticAction($2); }
	| ClassInstanceCreationExpression																		{ $$ = PrimaryCExpSemanticAction($1); }
	;

ClassInstanceCreationExpression: UnqualifiedClassInstanceCreationExpression																											{ $$ = InstanceCreationExpressionSemanticAction($1); }
	| VarAccess JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression																										{ $$ = VAccessInstanceCreationExpressionSemanticAction($1, $3); }
	| Primary JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression																											{ $$ = PrimaryInstanceCreationExpressionSemanticAction($1,$3); }
	;

UnqualifiedClassInstanceCreationExpression: JAVA_NEW param OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES																			{ $$ = UnqualifiedClassSemanticAction($2,$4); }
	;

Literal: NUMBER																																										{ $$ = JavaLiteralTokenSemanticAction($1); }
	| JAVA_TRUE																																										{ $$ = JavaLiteralTokenSemanticAction($1); }
	| JAVA_FALSE																																									{ $$ = JavaLiteralTokenSemanticAction($1); }
	| FLOAT																																											{ $$ = JavaLiteralTokenSemanticAction($1); }
	| STR																																											{ $$ = JavaLiteralStrSemanticAction($1); }
	;

%%
