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
	Type* type;
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
 * this approach for the AST root node ("program" non-terminal, in this
 * grammar), or it will drop the entire tree even if the parse succeeds.
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
%token <token> TOKEN_TYPE
%token <token> RANGER
%token <token> ENDLINE
%token <token> ARROW
%token <token> OPEN_BRACES
%token <token> CLOSE_BRACES
%token <token> OPEN_PARENTHESES
%token <token> CLOSE_PARENTHESES
%token <token> OPEN_BRACKET
%token <token> CLOSE_BRACKET
%token <token> PIPE

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
%token <token> JAVA_NOT_EXACT_COMPARISON
%token <token> JAVA_ASSIGNMENT
%token <token> JAVA_MULTIPLY_ASSIGN
%token <token> JAVA_DIVIDE_ASSIGN
%token <token> JAVA_MODULO_ASSIGN
%token <token> JAVA_PLUS_ASSIGN
%token <token> JAVA_MINUS_ASSIGN
%token <token> JAVA_LEFT_SHIFT_ASSIGN
%token <token> JAVA_RIGHT_SHIFT_ASSIGN
%token <token> JAVA_UNSIGNED_RIGHT_SHIFT_ASSIGN
%token <token> JAVA_AND_ASSIGN
%token <token> JAVA_XOR_ASSIGN
%token <token> JAVA_OR_ASSIGN

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
%token <string> NUMBER
%token <string> FLOAT
%token <token> UNKNOWN

/** Non-terminals. */


%type <program> program
%type <lexeme> lexeme
%type <lexeme_precursor> lexeme_precursor
%type <regex_class> regex_class
%type <symbol> symbol
%type <regexes> regexes
%type <action> action
%type <type> type
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
%left JAVA_EXACT_COMPARISON JAVA_NOT_EXACT_COMPARISON
%left JAVA_ASSIGNMENT JAVA_MULTIPLY_ASSIGN JAVA_DIVIDE_ASSIGN JAVA_MODULO_ASSIGN JAVA_PLUS_ASSIGN JAVA_MINUS_ASSIGN JAVA_LEFT_SHIFT_ASSIGN JAVA_RIGHT_SHIFT_ASSIGN JAVA_UNSIGNED_RIGHT_SHIFT_ASSIGN JAVA_AND_ASSIGN JAVA_XOR_ASSIGN JAVA_OR_ASSIGN   
%right JAVA_NOT              
%left UPPERCASE LOWERCASE DIGIT SYMBOL ESCAPED_SYMBOL
%left  OPEN_PARENTHESES
%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: ruleset																																						{ $$ = ProgramSemanticAction(currentCompilerState(), $1); }
	;

ruleset: rule ruleset																																					{ $$ = RulesetSemanticAction($1, $2); }
	| rule																																								{ $$ = RulesetSemanticAction($1, NULL); }
	;

rule: VAR_NAME[def] OPEN_BRACKET regexes[regex] CLOSE_BRACKET ENDLINE	    																							{ $$ = RuleNewRegexSemanticAction($def, $regex, currentCompilerState()); }
	| lexeme_precursor[lex] ARROW action ENDLINE																														{ $$ = RuleDefinitionSemanticAction($lex, $action, LEXEME_ACTION); }
	| lexeme_precursor[lex] ENDLINE																																		{ $$ = RuleDefinitionSemanticAction($lex, NULL, IGNORE_LEXEME); }
	;

lexeme_precursor: lexeme lexeme_precursor																																{ $$ = LexemePrecursorSemanticAction($1, $2, CONCATENATION); }
	| lexeme PIPE lexeme_precursor																																		{ $$ = LexemePrecursorSemanticAction($1, $3, SUMMATION); }
	| lexeme																																							{ $$ = LexemePrecursorSemanticAction($1, NULL, END); }
	| DEFAULT[string]																																					{ $$ = LexemeDefaultSemanticAction(); }
	;

lexeme: OPEN_BRACKET regexes[regex] CLOSE_BRACKET closure[closure_p]																									{ $$ = LexemeSemanticAction(NULL, $regex, $closure_p, REGEXES_TYPE, NULL); }
	| OPEN_BRACES VAR_NAME[id] CLOSE_BRACES closure[closure_p]																											{ $$ = LexemeSemanticAction($id, NULL, $closure_p, NAME, currentCompilerState()); }
	| OPEN_PARENTHESES lexeme_precursor[precursor] CLOSE_PARENTHESES closure[clos] 																						{ $$ = LexemeClosureSemanticAction($precursor, $clos);}
	| STR[string]																																						{ $$ = LexemeStringSemanticAction($string); }
	;

closure: %empty 																																						{ $$ = NULL; }
	| PLUS																																								{ $$ = ClosureSemanticAction($1); }
	| STAR																																								{ $$ = ClosureSemanticAction($1); }
	;

regexes: regex_class 																																					{ $$ = RegexesSemanticAction($1, NULL); }
	| regex_class regexes 																																				{ $$ = RegexesSemanticAction($1, $2); }
	;

regex_class: symbol																																						{ $$ = SymbolRegexSemanticAction($1); }
    | symbol RANGER symbol																																				{ $$ = RegexClassRangeSemanticAction($1, $3); }
	| OPEN_BRACES VAR_NAME[id] CLOSE_BRACES closure[clousure]																											{ $$ = CreatedClassSemanticAction($id, $clousure, currentCompilerState()); }
	;

symbol: LOWERCASE 																																						{ $$ = RegexSymbolSemanticAction($1); }
	| UPPERCASE 																																						{ $$ = RegexSymbolSemanticAction($1); }
	| DIGIT 																																							{ $$ = RegexSymbolSemanticAction($1); }
	| SYMBOL 																																							{ $$ = RegexSymbolSemanticAction($1); }
	| ESCAPED_SYMBOL 																																					{ $$ = RegexSymbolSemanticAction($1); }
	;

action: VAR_NAME																																						{ $$ = ActionSemanticAction($1); }
	| OPEN_BRACES Block[block] CLOSE_BRACES																																{ $$ = ActionJavaSemanticAction($block); }
	;

type: STRING_TYPE																																						{ $$ = TypeSemanticAction($1); }
    | INTEGER_TYPE																																						{ $$ = TypeSemanticAction($1); }
    | DOUBLE_TYPE																																						{ $$ = TypeSemanticAction($1); }
	| BOOLEAN_TYPE																																						{ $$ = TypeSemanticAction($1); }
	| TOKEN_TYPE																																						{ $$ = TypeSemanticAction($1); }
	;


/* Abandon all hope ye who enter here
** Closely based on https://docs.oracle.com/javase/specs/jls/se17/jls17.pdf, an attempt at defining a syntactic grammar for Java
** This version is significantly reduced and simplified
**
** Note: Statement is the StatementNoShortIf following the model in 'The Java® Language Specification Java SE 17 Edition', no if statements without braces are accepted.
** Extending java parsing beyond this point far extends the scope of this program.
*/

NumericComparison: JAVA_GEQ																																				{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_GREATER																																						{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_LEQ																																							{ $$ = JavaNumericComparisonSemanticAction($1); }
	| JAVA_LESSER																																						{ $$ = JavaNumericComparisonSemanticAction($1); }
	;
;
Block: Statement Block																																					{ $$ = JavaBlockSemanticAction($1, $2); }
	| Statement																																							{ $$ = JavaBlockSemanticAction($1, NULL); }
	| JAVA_RETURN Expression ENDLINE																																	{ $$ = JavaReturnExpressionSemanticAction($2); }
	| JAVA_THROW Expression ENDLINE																																		{ $$ = JavaThrowExpressionSemanticAction($2); }
	;

Statement: ENDLINE																																						{ $$ = NULL; }
	| StatementExpression  ENDLINE																																		{ $$ = JavaStatementExpressionSemanticAction($1); }
	| IfThenStatement																																					{ $$ = IfStatementSemanticAction($1); }
	| JAVA_WHILE OPEN_PARENTHESES Expression[exp] CLOSE_PARENTHESES OPEN_BRACES Block[state] CLOSE_BRACES																{ $$ = WhileStatementSemanticAction($exp, $state); }
	| JAVA_FOR OPEN_PARENTHESES ForInit[init] ENDLINE Expression[exp] ENDLINE StatementExpressionList[stlist] CLOSE_PARENTHESES OPEN_BRACES Block[state] CLOSE_BRACES	{ $$ = ForStatementSemanticAction($init, $exp, $stlist, $state); }
	;

ForInit: StatementExpressionList																																		{ $$ = ForInitExpressionListSemanticAction($1); }
	| type VAR_NAME																																						{ $$ = JavaVarTypeDefinitionSemantictAction($1, $2, WITH_TYPES, currentCompilerState()); }
	;

StatementExpressionList: %empty																																			{ $$ = NULL; }
	| StatementExpression																																				{ $$ = StatementExpressionListSemanticAction($1, NULL); }
	| StatementExpression COMMA StatementExpressionList																													{ $$ = StatementExpressionListSemanticAction($1, $3); }
	;

IfThenStatement: JAVA_IF OPEN_PARENTHESES Expression[expression] CLOSE_PARENTHESES OPEN_BRACES Block[ifblock] CLOSE_BRACES												{ $$ = JavaIfThenStructureSemanticAction($expression, $ifblock, NULL); }
	| JAVA_IF OPEN_PARENTHESES Expression[expression] CLOSE_PARENTHESES OPEN_BRACES Block[ifblock] CLOSE_BRACES JAVA_ELSE OPEN_BRACES Block[elseblock] CLOSE_BRACES		{ $$ = JavaIfThenStructureSemanticAction($expression, $ifblock, $elseblock); }
	;

StatementExpression: Assignment																																			{ $$ = JavaAsignmentSemanticAction($1); }
	| VarAccess																																							{ $$ = JavaVAccessDefaultSemanticAction($1); }
	| type VAR_NAME JAVA_ASSIGNMENT Expression																															{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_PLUS_ASSIGN Expression																															{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_MINUS_ASSIGN Expression																														{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_MULTIPLY_ASSIGN Expression																														{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_DIVIDE_ASSIGN Expression																														{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_MODULO_ASSIGN Expression																														{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_LEFT_SHIFT_ASSIGN Expression																													{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_RIGHT_SHIFT_ASSIGN Expression																													{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_UNSIGNED_RIGHT_SHIFT_ASSIGN Expression																											{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_AND_ASSIGN Expression																															{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_XOR_ASSIGN Expression																															{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	| type VAR_NAME JAVA_OR_ASSIGN Expression																															{ $$ = JavaAsignmentTypeSemanticAction($1, $2, $3, $4); }
	;


VarAccess: VAR_NAME																																						{ $$ = VarAccessVarSemanticAction($1); }
	| VAR_NAME JAVA_DOT_OPERATOR VarAccess																																{ $$ = VarAccessVarOperatorSemanticAction($1,$3); }
	| type JAVA_DOT_OPERATOR VarAccess																																	{ $$ = VarAccessTypeOperatorSemanticAction($1,$3); }
	| MethodInvocation																																					{ $$ = VarAccessMethodInvocationSemanticAction($1); }
	;

MethodInvocation: VarAccess OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES																								{ $$ = InvocationSemanticAction($1, $3 ); }
	;

ArgumentList: %empty																																					{ $$ = NULL; }
	| Expression																																						{ $$ = ArgListSemanticExpression($1,NULL); }
	| Expression COMMA ArgumentList																																		{ $$ = ArgListSemanticExpression($1,$3); }
	;

Expression: ConditionalExpression																																		{ $$ = expressionSematicAction($1, NULL); }
	| Assignment																																						{ $$ = expressionSematicAction(NULL, $1); }
	;

ConditionalExpression: ConditionalOrExpression																															{ $$ = JavaConditionalExpSemanticAction($1,NULL,NULL); }
	| ConditionalOrExpression JAVA_TERNARY_OPERATOR Expression JAVA_DOTS_OPERATOR ConditionalExpression																	{ $$ = JavaConditionalExpSemanticAction($1, $3, $5); }
	;

ConditionalOrExpression: ConditionalAndExpression																														{ $$ = JavaConditionalOrExpressionSemanticAction($1, NULL); }
	| ConditionalOrExpression JAVA_OR ConditionalAndExpression																											{ $$ = JavaConditionalOrExpressionSemanticAction($3, $1 ); }
	;

ConditionalAndExpression: EqualityExpression																															{ $$ = JavaConditionalAndExpressionSemanticAction(NULL, $1); }
	| ConditionalAndExpression JAVA_AND EqualityExpression                                                        														{ $$ = JavaConditionalAndExpressionSemanticAction($1,$3); }
	;

EqualityExpression: UnaryExpression																																		{ $$ = EqualityExpressionSemanticAction($1,  0, NULL); }
	| EqualityExpression JAVA_EXACT_COMPARISON UnaryExpression																											{ $$ = EqualityExpressionSemanticAction($3,$2,$1); }
	| EqualityExpression JAVA_NOT_EXACT_COMPARISON UnaryExpression																										{ $$ = EqualityExpressionSemanticAction($3,$2,$1); }
	;

UnaryExpression:  UnaryExpression NumericComparison PostfixExpression																									{ $$ = UnaryExpressionNumericComparisonSintaticAction($1, $2, $3); }
	| UnaryExpression STAR PostfixExpression																															{ $$ = UnaryExpressionDoubleTokenSintaticAction($1, STAR_TYPE, $3); }
	| UnaryExpression DIV PostfixExpression																																{ $$ = UnaryExpressionDoubleTokenSintaticAction($1, DIV_TYPE, $3); }
	| UnaryExpression MOD PostfixExpression																																{ $$ = UnaryExpressionDoubleTokenSintaticAction($1, MOD_TYPE, $3); }
	| UnaryExpression PLUS PostfixExpression																															{ $$ = UnaryExpressionDoubleTokenSintaticAction($1, PLUS_TYPE, $3); }
	| UnaryExpression MINUS PostfixExpression																															{ $$ = UnaryExpressionDoubleTokenSintaticAction($1, MINUS_TYPE, $3); }
	| PostfixExpression																																					{ $$ = UnaryExpressionPostfixExpressionSintaticAction($1); }
	| JAVA_NOT UnaryExpression																																			{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| OPEN_PARENTHESES type CLOSE_PARENTHESES																															{ $$ = UnaryExpressionTypeSintaticAction($2); }
	| OPEN_PARENTHESES CLOSE_PARENTHESES																																{ $$ = UnaryExpressionTypeSintaticAction(NULL); }
	| DECREMENT UnaryExpression																																			{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| MINUS UnaryExpression																																				{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| INCREMENT UnaryExpression																																			{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	| PLUS UnaryExpression																																				{ $$ = UnaryExpressionSingleTokenSintaticAction($2,$1); }
	;


PostfixExpression: Primary																																				{ $$ = PostfixExpressionPrimarySemanticAction($1); }
	| VarAccess																																							{ $$ = PostfixExpressionVAccessDefaultSemanticAction($1); }
	| VarAccess INCREMENT																																				{ $$ = PostfixExpressionVAccessSemanticAction($1,$2); }
	| VarAccess DECREMENT																																				{ $$ = PostfixExpressionVAccessSemanticAction($1, $2); }
	;

Assignment: VarAccess JAVA_ASSIGNMENT Expression																														{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_PLUS_ASSIGN Expression																																{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_MINUS_ASSIGN Expression																															{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_MULTIPLY_ASSIGN Expression																															{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_DIVIDE_ASSIGN Expression																															{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_MODULO_ASSIGN Expression																															{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_LEFT_SHIFT_ASSIGN Expression																														{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_RIGHT_SHIFT_ASSIGN Expression																														{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_UNSIGNED_RIGHT_SHIFT_ASSIGN Expression																												{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_AND_ASSIGN Expression																																{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_XOR_ASSIGN Expression																																{ $$ = AssignmentSemanticAction($1, $2, $3); }
	| VarAccess JAVA_OR_ASSIGN Expression																																{ $$ = AssignmentSemanticAction($1, $2, $3); }
	;


Primary: Literal																																						{ $$ = PrimaryLiteralSemanticAction($1); }
	| OPEN_PARENTHESES Expression CLOSE_PARENTHESES																														{ $$ = PrimaryExpressionSemanticAction($2); }
	| ClassInstanceCreationExpression																																	{ $$ = PrimaryCExpSemanticAction($1); }
	;

ClassInstanceCreationExpression: UnqualifiedClassInstanceCreationExpression																								{ $$ = InstanceCreationExpressionSemanticAction($1); }
	| VarAccess JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression																							{ $$ = VAccessInstanceCreationExpressionSemanticAction($1, $3); }
	| Primary JAVA_DOT_OPERATOR UnqualifiedClassInstanceCreationExpression																								{ $$ = PrimaryInstanceCreationExpressionSemanticAction($1,$3); }
	;

UnqualifiedClassInstanceCreationExpression: JAVA_NEW type OPEN_PARENTHESES ArgumentList CLOSE_PARENTHESES																{ $$ = UnqualifiedClassSemanticAction($2,$4); }
	| JAVA_NEW MethodInvocation																																			{ $$ = UnqualifiedClassSemanticActionInvocation($2); }
	;

Literal: NUMBER																																							{ $$ = JavaLiteralStrSemanticAction($1); }
	| JAVA_TRUE																																							{ $$ = JavaLiteralTokenSemanticAction($1); }
	| JAVA_FALSE																																						{ $$ = JavaLiteralTokenSemanticAction($1); }
	| FLOAT																																								{ $$ = JavaLiteralStrSemanticAction($1); }
	| STR																																								{ $$ = JavaLiteralStrSemanticAction($1); }
	;

%%
