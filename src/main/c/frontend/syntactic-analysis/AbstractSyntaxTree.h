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
};

struct Param{
	char* stuff;
};

typedef enum {
	kleene,
	positive,
	empty
} Closure_type;

struct Closure{
	Closure_type type;
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
	struct Regex_class* regex_class;
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
	regex,
	default
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
			Closure* closure;
		};
		Token* def;
	};
	Lexeme_type type;
};

typedef enum {
	regex,
	ignore_lexeme,
	lexeme_action
} Ruleset_type;


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

	};
	Token* endline;
	Ruleset_type type;
};

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


