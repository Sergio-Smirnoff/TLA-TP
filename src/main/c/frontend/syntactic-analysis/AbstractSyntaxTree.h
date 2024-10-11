#ifndef ABSTRACT_SYNTAX_TREE_HEADER
#define ABSTRACT_SYNTAX_TREE_HEADER

#include "../../shared/Logger.h"
#include <stdlib.h>

/** Initialize module's internal state. */
void initializeAbstractSyntaxTreeModule();

/** Shutdown module's internal state. */
void shutdownAbstractSyntaxTreeModule();

/**
 * This typedefs allows self-referencing types.
 */

typedef enum RegexType RegexType;

typedef struct Range Range;
typedef struct Program Program;
typedef struct Closure Closure;
typedef struct Param Param;
typedef struct Lexeme Lexeme;
typedef struct Action Action;
typedef struct Ruleset Ruleset;
typedef struct Rule Rule;
typedef struct Function_body Function_body;
typedef struct Constant Constant;
typedef struct Expression Expression;
typedef struct Factor Factor;
/**
 * Node types for the Abstract Syntax Tree (AST).
 */

struct Function_body{
	char* log;
	char* ret;
};

struct Param{
	char* stuff;
};

struct Closure{
	char* closure;
};

typedef enum {
	stuff,
	range
} Regex_class_type;

typedef struct Regex_class {
	union {
		char* stuff;
		Range* range;
	};
	Regex_class* regex_class;
	Regex_class_type type;
} Regex_class;

typedef enum {
	lowercase,
	uppercase,
	number,
	both
} Range_type;

struct Range {
    union {
        Token left_lowercase;
        Token left_uppercase;
        Token left_number;
    } left;  
	char * range;
    union {
        Token right_lowercase;
        Token right_uppercase;
        Token right_number;
    } right; 

    Range_type type;
};

typedef enum  {
	action,
	function_body
} Action_type;

struct Action {
	union{
		char* action;
		struct{
			char* function_body;
			Param* param;
		};
	};
	Action_type type;
};

typedef enum  {
	string,
	regex_class,
	reg,
	def
} Lexeme_type;

struct Lexeme{
	union{
		char* string;
		struct{
			Regex_class* regex_class;
			Closure* closure;
		};
		struct{
			char* our_regex_id;
			Closure* clre;
		};
		Token* def;
	};
	Lexeme_type type;
};

typedef enum {
	regex,
	ignore_lexeme,
	lexeme_action
} Rule_type;

struct Rule {
	union{
		struct{
			char* our_regex_id;
			Regex_class* regex_class;
		};
		Lexeme* lexeme;
		struct{
			Lexeme* lex;
			Action* action;
		};

	};
	Token* endline;
	Rule_type type;
};

struct Ruleset{
	Rule* rule;
	Ruleset* ruleset;
};

struct Program {
	Ruleset * ruleset;
};

/**
 * Node recursive destructors.
 */

void releaseProgram(Program * program);
void releaseRuleset(Ruleset * ruleset);
void releaseRule(Rule * rule);
void releaseLexeme(Lexeme * lexeme);
void releaseAction(Action * action);
void releaseRegexClass(Regex_class * regex_class);
void releaseRange(Range * range);
void releaseClosure(Closure * closure);
void releaseParam(Param * param);
//void releaseFunctionBody(Function_body * function_body);


#endif


