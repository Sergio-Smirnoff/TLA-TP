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


typedef struct Action Action;
typedef struct Regex Regex;
typedef struct Program Program;

/**
 * Node types for the Abstract Syntax Tree (AST).
 */

enum Ruleset_type {
	regex,
	lexeme,
	lexeme_action
};

enum Action_type {
	action,
	function_body
};

enum Range_type {
	lowercase,
	uppercase,
	number
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

struct Action {
	char* action;
	Param* param;
	Action_type type;
}

struct Param{
	char* stuff;
}

struct Ruleset {
	char* our_regex_id;
	union{
		Regex_class regex_class;
		Lexeme* lexeme;
	}
	Action* action;
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


