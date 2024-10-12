%{

#include "BisonActions.h"

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

/**%destructor { releaseFunctionBody($$); } <function_body>*/
%destructor { releaseClosure($$); } <closure>
%destructor { releaseParam($$); } <param>
%destructor { releaseRange($$); } <range>
%destructor { releaseRegexClass($$); } <regex_class>
%destructor { releaseAction($$); } <action>
%destructor { releaseLexeme($$); } <lexeme>
%destructor { releaseRule($$); } <rule>
%destructor { releaseRuleset($$); } <ruleset>
%destructor { releaseProgram($$); } <program>


/** Terminals. */
%token <string> DIGIT
%token <string> LOWERCASE
%token <string> UPPERCASE
%token <string> SYMBOL
%token <string> ESCAPED_SYMBOL
%token <string> OUR_REGEX_ID
%token <string> STR
%token <string> VAR_NAME
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
%token <token> ARROW
%token <token> OPEN_BRACES
%token <token> CLOSE_BRACES
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

ruleset: rule ruleset											{$$ = RulesetSemanticAction( $1, $2); }
	| rule														{$$ = RulesetSemanticAction( $1, NULL); }
	;

rule: OUR_REGEX_ID[def] regex_class[regex] ENDLINE[endline]	    { $$ = RuleNewRegexSemanticAction($def, $regex, $endline); }
	| lexeme[lex] ARROW action ENDLINE[endline]				{ $$ = RuleDefinitionSemanticAction( $lex, $2, $endline, lexeme_action); }
	| lexeme[lex] ENDLINE[endline]								{ $$ = RuleDefinitionSemanticAction( $lex, NULL, $endline, ignore_lexeme ); }
	;

lexeme: STR[string]														{ $$ = LexemeSemanticAction( $string, NULL, NULL, string); }					
	| OPEN_BRACES regex_class[regex] closure								{ $$ = LexemeSemanticAction( NULL, $regex, $3, regex_class); }
	| OPEN_BRACES OUR_REGEX_ID[id] closure								{ $$ = LexemeSemanticAction( $id, NULL, $2, reg); }
	| DEFAULT[string]													{ $$ = LexemeSemanticAction( $string, NULL, NULL, def); }
	;

closure: KLEENE													 		{ $$ = ClosureSemanticAction($1); }
	| POSITIVE													 		{ $$ = ClosureSemanticAction($1); }					
	| CLOSE_BRACES
	; 

regex_class: LOWERCASE									{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | UPPERCASE											{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | DIGIT												{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| SYMBOL											{ $$ = RegexClassStringSemanticAction($1, NULL); }
	| ESCAPED_SYMBOL									{ $$ = RegexClassStringSemanticAction($1, NULL); }
    | range												{ $$ = RegexClassRangeSemanticAction($1, NULL); }
	| LOWERCASE regex_class								{ $$ = RegexClassStringSemanticAction($1, $2); }
    | UPPERCASE regex_class								{ $$ = RegexClassStringSemanticAction($1, $2); }
    | DIGIT regex_class									{ $$ = RegexClassStringSemanticAction($1, $2); }
    | range regex_class									{ $$ = RegexClassRangeSemanticAction($1, $2); }
	| SYMBOL regex_class								{ $$ = RegexClassStringSemanticAction($1, $2); }
	| ESCAPED_SYMBOL regex_class						{ $$ = RegexClassStringSemanticAction($1, $2); }
	;

range: LOWERCASE RANGER LOWERCASE									{ $$ = RangeSemanticAction($1, $3); }
    | UPPERCASE RANGER UPPERCASE										{ $$ = RangeSemanticAction($1, $3); }
    | DIGIT RANGER DIGIT												{ $$ = RangeSemanticAction($1, $3); }
	| UPPERCASE RANGER LOWERCASE										{ $$ = RangeSemanticAction($1, $3); }
	;

action: ACTION												{ $$ = ActionSemanticAction($1); }
	| param FUNCTION_BODY									{ $$ = ActionParamSemanticAction($1, $2); }
	;
/*
function_body: LOG
	| LOG RETURN
	| RETURN
	;
*/
param: STRING_TYPE					{ $$ = ParamSemanticAction($1); }
    | INTEGER_TYPE					{ $$ = ParamSemanticAction($1); }
    | DOUBLE_TYPE					{ $$ = ParamSemanticAction($1); }
	| BOOLEAN_TYPE					{ $$ = ParamSemanticAction($1); }
	| %empty
	;

%%
