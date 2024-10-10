%{

#include "BisonActions.h"

%}

// You touch this, and you die.
%define api.value.union.name SemanticValue

%union {
	/** Terminals. */

	char* string;
	Token token;

	/** Non-terminals. */

	Regex * regex;
	Program * program;
}

/**
 * Destructors. This functions are executed after the parsing ends, so if the
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

/** Terminals. */
%token <string> REGEX
%token <string> ACTION_ID
%token <string> ACTION_DEF
%token <string> OPEN_BRACKETS
%token <string> CLOSE_BRACKETS
%token <string> RETURN_TOKEN
%token <string> REGEX_NAME

%token <string> REGEX_RANGE_START
%token <string> REGEX_RANGE_END
%token <string> REGEX_LITERAL

%token <string


/** Non-terminals. */

%type <regex> regex
%type <program> program
%type <ruleset> ruleset
%type <rule> rule
%type <function_definition> function_definition
%type <java_function> java_function

%type <regex_definition> regex_definition
%type <regex_content> regex_content
%type <regex_range> regex_range

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

ruleset: ruleset ruleset
	| regex_definition
	| rule;

rule: REGEX function_definition;

function_definition: RETURN_TOKEN
	| java_function;


regex_definition: REGEX_NAME regex_content;

regex_content: regex_content regex_content
	| REGEX_LITERAL
	| regex_range;

regex_range: REGEX_RANGE_START
	| REGEX_RANGE_END;


java_function: OPEN_BRACKETS %empty CLOSE_BRACKETS;

regex: REGEX ACTION_ID									{ $$ = RegexSemanticAction($1, $2, ID); }
	| REGEX ACTION_DEF									{ $$ = RegexSemanticAction($1, $2, DEF); }
	;

%%
