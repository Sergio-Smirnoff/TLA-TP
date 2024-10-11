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


typedef struct Range Range;
typedef struct Program Program;

/**
 * Node types for the Abstract Syntax Tree (AST).
 */

struct Function_body{
	char* log;
	char* ret;
}

struct Param{
	char* stuff;
}
enum Closure_type {
	kleene,
	positive,
	empty
};

struct Closure{
	Closure_type type;
}

enum Regex_class_type{
	stuff,
	range
}

struct Regex_class {
	union {
		char* stuff;
		Range* range;
	}
	Regex_class* regex_class;
	Regex_class_type type;
}

enum Range_type {
	lowercase,
	uppercase,
	number,
	both
};

struct Range {
	union {
		Token lowercase;
		Token uppercase;
		Token number
	}
	char* range;
	union {
		Token lowercase;
		Token uppercase;
		Token number;
	}
	Range_type type;
}

enum Action_type {
	action,
	function_body
};

struct Action {
	union{
		char* action;
		struct{
			char* function_body;
			Param* param;
		}
	}
	Action_type type;
}

enum Lexeme_type {
	string,
	regex_class,
	regex,
	default
}

struct Lexeme{
	union{
		char* string;
		struct{
			Regex_class* regex_class;
			Closure* closure;
		};
		struct{
			char* our_regex_id;
			Closure* closure;
		};
		Token* def;
	}
	Lexeme_type type;
}

enum Ruleset_type {
	regex,
	ignore_lexeme,
	lexeme_action
};


struct Ruleset {
	union{
		struct{
			char* our_regex_id;
			Regex_class* regex_class;
		};
		Lexeme* lexeme;
		struct{
			Lexeme* lexeme;
			Action* action;
		};

	}
	Token* endline;
	Ruleset_type type;
}

struct Program {
	Ruleset * ruleset;
};

/**
 * Node recursive destructors.
 */
/*
void releaseConstant(Constant * constant);
void releaseExpression(Expression * expression);
void releaseFactor(Factor * factor);
*/

void releaseRegex(Regex * regex);
void releaseProgram(Program * program);

#endif


