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
%token <string> ACTION
%token <string> FUNCTION_BODY
%token <string> DEFAULT

//%token <token> LOG
//%token <token> RETURN
%token <token> KLEENE
%token <token> POSITIVE
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

ruleset: rule ruleset											{$$ = NULL; }
	| rule														{$$ = NULL; }
	;

rule: VAR_NAME[def] regex_class[regex] ENDLINE[endline]	    { $$ = NULL; }
	| lexeme[lex] ARROW action[act] ENDLINE[endline]				{ $$ = NULL; }
	| lexeme[lex] ENDLINE[endline]								{ $$ = NULL; }
	;

lexeme: STR[string]														{ $$ = NULL; }					
	| OPEN_BRACES regex_class[regex] closure								{ $$ = NULL; }
	| OPEN_BRACES VAR_NAME[id] closure								{ $$ = NULL; }
	| DEFAULT[string]													{ $$ = NULL; }
	;

closure: KLEENE													 		{ $$ = NULL; }
	| POSITIVE													 		{ $$ = NULL; }					
	| CLOSE_BRACES														{ $$ = NULL; }	
	; 

regex_class: LOWERCASE									{ $$ = NULL; }
    | UPPERCASE											{ $$ = NULL; }
    | DIGIT												{ $$ = NULL; }
	| SYMBOL											{ $$ = NULL; }
	| ESCAPED_SYMBOL									{ $$ = NULL; }
    | range												{ $$ = NULL; }
	| LOWERCASE regex_class								{ $$ = NULL; }
    | UPPERCASE regex_class								{ $$ = NULL; }
    | DIGIT regex_class									{ $$ = NULL; }
    | range regex_class									{ $$ = NULL; }
	| SYMBOL regex_class								{ $$ = NULL; }
	| ESCAPED_SYMBOL regex_class						{ $$ = NULL; }
	;

range: LOWERCASE RANGER LOWERCASE										{ $$ = NULL; }
    | UPPERCASE RANGER UPPERCASE										{ $$ = NULL; }
    | DIGIT RANGER DIGIT												{ $$ = NULL; }
	| UPPERCASE RANGER LOWERCASE										{ $$ = NULL; }
	;

action: VAR_NAME															{ $$ = NULL; }
	| OPEN_PARENTHESES param FUNCTION_BODY									{ $$ = NULL; }
	;
/*
function_body: LOG
	| LOG RETURN
	| RETURN
	;
*/
param: STRING_TYPE					{ $$ = NULL; }
    | INTEGER_TYPE					{ $$ = NULL; }
    | DOUBLE_TYPE					{ $$ = NULL; }
	| BOOLEAN_TYPE					{ $$ = NULL; }
	| %empty						{ $$ = NULL; }
	;

%%
