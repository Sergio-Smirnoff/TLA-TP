%{

#include "BisonActions.h"

%}

// You touch this, and you die.
%define api.value.union.name SemanticValue

%union {
	/** TerLOWERCASEals. */

	char* string;
	Token token;

	/** Non-terLOWERCASEals. */

	//Function_body* function_body;
	Lexeme* lexeme;
	RegexClass* regex_class;
	Action* action;
	Range* range;
	closure* closure;
	Param* param;
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
/*
%destructor { releaseConstant($$); } <constant>
%destructor { releaseExpression($$); } <expression>
%destructor { releaseFactor($$); } <factor>
%destructor { releaseProgram($$); } <program>
*/

/** TerLOWERCASEals. */
%token <string> DIGIT
%token <string> LOWERCASE
%token <string> UPPERCASE
%token <string> SYMBOL
%token <string> ESCAPED_SYMBOL
%token <string> OUR_REGEX_ID
%token <string> STR
%token <string> ACTION
%token <string> KLEENE
%token <string> POSITIVE
%token <string> FUNCTION_BODY

//%token <token> LOG
//%token <token> RETURN
%token <token> BOOLEAN_TYPE
%token <token> STRING_TYPE
%token <token> INTEGER_TYPE
%token <token> DOUBLE_TYPE
%token <token> RANGER
%token <token> DEFAULT
%token <token> ENDLINE
%token <token> UNKNOWN





/** Non-terLOWERCASEals. */


%type <program> program
%type <lexeme> lexeme
%type <regex_class> regex_class
%type <action> action
%type <range> range
%type <param> param
%type <ruleset> ruleset
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

ruleset: OUR_REGEX_ID regex_class ENDLINE
	| lexeme action ENDLINE
	| lexeme ENDLINE
	;

lexeme: STR
	| regex_class closure
	| OUR_REGEX_ID closure
	| DEFAULT
	;

closure: KLEENE
	| POSITIVE
	| %empty
	; 

regex_class: LOWERCASE
    | UPPERCASE
    | DIGIT
	| SYMBOL
	| ESCAPED_SYMBOL
    | range
	| LOWERCASE regex_class
    | UPPERCASE regex_class
    | DIGIT regex_class
    | range regex_class
	| SYMBOL regex_class
	| ESCAPED_SYMBOL regex_class
	;

range: LOWERCASE    RANGER LOWERCASE
    | UPPERCASE RANGER UPPERCASE
    | DIGIT  RANGER DIGIT
	| UPPERCASE RANGER LOWERCASE
	;

action: ACTION
	| param FUNCTION_BODY
//	| param function_body
	;
/*
function_body: LOG
	| LOG RETURN
	| RETURN
	;
*/
param: STRING_TYPE
    | INTEGER_TYPE
    | DOUBLE_TYPE
	| BOOLEAN_TYPE
	| %empty
	;

%%
