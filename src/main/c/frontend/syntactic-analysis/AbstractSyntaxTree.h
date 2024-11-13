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

typedef struct Program Program;
typedef struct Closure Closure;
typedef struct Type Type;
typedef struct Lexeme Lexeme;
typedef struct Lexeme_precursor Lexeme_precursor;
typedef struct Action Action;
typedef struct Ruleset Ruleset;
typedef struct Rule Rule;
typedef struct Function_body Function_body;
typedef struct Regex_class Regex_class;
typedef struct Java_function_body Java_function_body;

typedef struct Regexes Regexes;
typedef struct Symbol Symbol;

typedef struct NumericComparison NumericComparison;
typedef struct Statement Statement;
typedef struct Block Block;
typedef struct ForInit ForInit;
typedef struct StatementExpressionList StatementExpressionList;
typedef struct IfThenStatement IfThenStatement;
typedef struct StatementExpression StatementExpression;
typedef struct VarAccess VarAccess;
typedef struct MethodInvocation MethodInvocation;
typedef struct ArgumentList ArgumentList;
typedef struct Expression Expression;
typedef struct ConditionalExpression ConditionalExpression;
typedef struct ConditionalOrExpression ConditionalOrExpression;
typedef struct ConditionalAndExpression ConditionalAndExpression;
typedef struct EqualityExpression EqualityExpression;
typedef struct UnaryExpression UnaryExpression;
typedef struct PostfixExpression PostfixExpression;
typedef struct Assignment Assignment;
typedef struct Primary Primary;
typedef struct ClassInstanceCreationExpression ClassInstanceCreationExpression;
typedef struct UnqualifiedClassInstanceCreationExpression UnqualifiedClassInstanceCreationExpression;
typedef struct Literal Literal;

/**
 * Node types for the Abstract Syntax Tree (AST).
 */

struct NumericComparison
{
	Token token;
};

typedef enum Block_type
{
	statement,
	throw,
	ret
} Block_type;

struct Block
{
	union
	{
		struct
		{
			Statement *statement;
			Block *block;
		};
		Expression *exp;
	};
	Block_type type;
};

typedef enum Statement_type
{
	state,
	ifThenStatement,
	While,
	For
} Statement_type;

struct Statement
{
	union
	{
		StatementExpression *sexp;
		IfThenStatement *ifThen;
		struct
		{
			Expression *expwhile;
			Block *blockwhile;
		};
		struct
		{
			ForInit *forInit;
			Expression *expfor;
			Block *blockfor;
			StatementExpressionList *statementExpList;
		};
	};
	Statement_type type;
};

typedef enum ForInitType
{
	statementExpList,
	withTypes,
	withoutTypes
} ForInitType;

struct ForInit
{
	union
	{
		StatementExpressionList *statementExpList;
		struct
		{
			Type *type;
			char *var_name_type;
		};
		char *var_name;
	};
	ForInitType for_type;
};

struct StatementExpressionList
{
	StatementExpression *exp;
	StatementExpressionList *list;
};

struct IfThenStatement
{
	Expression *exp;
	Block *ifblock;
	Block *elseblock;
};

typedef enum StatementExpressionType
{
	assignation,
	vaccess,
	assigType,
} StatementExpressionType;

struct StatementExpression
{
	union
	{
		Assignment *assignment;
		VarAccess *var_access;
		struct
		{
			Type *type;
			char *var_name;
			Expression *exp;
		};
	};
	StatementExpressionType state_type;
};

struct VarAccess
{
	char *var_name;
	VarAccess *vaccess;
	Token token;
	Type *type;
	MethodInvocation *method_invocation;
};

struct MethodInvocation
{
	VarAccess *vaccess;
	ArgumentList *arglist;
};

struct ArgumentList
{
	Expression *expression;
	ArgumentList *arglist;
};

typedef enum Expression_type
{
	xexp,
	assignment
} Expression_type;

struct Expression
{
	union
	{
		ConditionalExpression *xexp;
		Assignment *assignment;
	};
	Expression_type type;
};

struct ConditionalExpression
{
	ConditionalOrExpression *corexp;
	Expression *exp;
	ConditionalExpression *cexp;
};

struct ConditionalOrExpression
{
	ConditionalAndExpression *candexp;
	ConditionalOrExpression *corexp;
};

struct ConditionalAndExpression
{
	EqualityExpression *eqexp;
	ConditionalAndExpression *candexp;
};

struct EqualityExpression
{
	UnaryExpression *uexp;
	EqualityExpression *eqexp;
};

typedef enum GlobalUnaryExpressionType
{
	numericComparison,
	doubleToken,
	postfixExpression,
	type,
	singleToken
} GlobalUnaryExpressionType;

typedef enum UnaryExpressionType
{
	star_t,
	div_type,
	mod_t,
	plus_t,
	minus_t
} UnaryExpressionType;

struct UnaryExpression
{
	union
	{
		struct
		{
			UnaryExpression *uexp1_num;
			NumericComparison *numcomp;
			PostfixExpression *uexp2_num;
		};
		struct
		{
			UnaryExpression *uexp1_exp;
			UnaryExpressionType type;
			PostfixExpression *uexp2_exp;
		};
		PostfixExpression *pexp;
		Type *obj_type;
		struct
		{
			Token token;
			UnaryExpression *uexp;
		};
	};
	GlobalUnaryExpressionType globaltype;
};

struct PostfixExpression
{
	Primary *primary;
	VarAccess *vaccess;
	Token token;
};

struct Assignment
{
	VarAccess *vaccess;
	Expression *expression;
	Token token;
};

typedef enum PrimaryType
{
	literal,
	expression,
	cexp
} PrimaryType;

struct Primary
{
	union
	{
		Literal *lit;
		Expression *exp;
		ClassInstanceCreationExpression *cice;
	};
	PrimaryType type;
};

struct ClassInstanceCreationExpression
{
	UnqualifiedClassInstanceCreationExpression *ucice;
	VarAccess *vaccess;
	Primary *primary;
};

typedef enum UnqualifiedClassInstanceCreationExpression_type
{
	method,
	parargs
} UnqualifiedClassInstanceCreationExpression_type;

struct UnqualifiedClassInstanceCreationExpression
{
	union
	{
		struct
		{
			Type *type;
			ArgumentList *arglist;
		};
		MethodInvocation *invocation;
	};
	UnqualifiedClassInstanceCreationExpression_type unq_type;
};

typedef enum Literal_type
{
	str,
	token
} Literal_type;

struct Literal
{
	union
	{
		char *str;
		Token token;
	};
	Literal_type type;
};

struct Function_body
{
	char *log;
	char *ret;
};

typedef enum Lexeme_type
{
	regexes,
	name,
	string_lexeme,
	precursor_closure
} Lexeme_type;

struct Lexeme
{
	union
	{
		Regexes *regexes;
		char *our_regex_id;
		char *string;
		Lexeme_precursor *precursor;
	};
	Closure *closure;
	Lexeme_type type;
};

typedef enum Lexeme_precursor_type
{
	default_t,
	nonliterals
} Lexeme_precursor_type;

typedef enum Lexeme_chain_type{
	concatenation,
	summation
} Lexeme_chain_type;

struct Lexeme_precursor
{
	Lexeme_precursor *lex_prec;
	Lexeme *lex;
	Lexeme_precursor_type precursor_type;
	Lexeme_chain_type chain_type;
};

struct Type
{
	Token stuff;
};

struct Closure
{
	Token closure;
};

struct Regexes
{
	Regex_class *regexClass;
	Regexes *regexes;
};

struct Symbol
{
	char *symbol_tok;
};

typedef enum Regex_class_type
{
	symbol,
	range,
	variable
} Regex_class_type;

typedef struct Regex_class
{
	union
	{
		struct
		{
			Symbol *startSymbol;
			Symbol *endSymbol;
		};
		struct
		{
			char *varName;
			Closure *closure;
		};
		Symbol *symbol;
	};
	Regex_class_type type;
} Regex_class;

typedef enum Action_type
{
	action,
	function_body
} Action_type;

struct Action
{
	union
	{
		char *varName;
		Block *block;
	};
	Action_type type;
};

typedef enum Rule_type
{
	regex,
	ignore_lexeme,
	lexeme_action
} Rule_type;

struct Rule
{
	union
	{
		struct
		{
			char *our_regex_id;
			Regexes *regexes;
		};
		Lexeme_precursor *lexeme;
		struct
		{
			Lexeme_precursor *lex;
			Action *action;
		};
	};
	Rule_type type;
};

struct Ruleset
{
	Rule *rule;
	Ruleset *ruleset;
};

struct Program
{
	Ruleset *ruleset;
};

/**
 * Node recursive destructors.
 */
/*
void releaseProgram(Program * program);
void releaseRuleset(Ruleset * ruleset);
void releaseRule(Rule * rule);
void releaseLexeme(Lexeme * lexeme);
void releaseAction(Action * action);
void releaseRegexClass(Regex_class * regex_class);
void releaseClosure(Closure * closure);
void releaseType(Type * type);
void releaseLexemePrecursor(Lexeme_precursor * lexeme_precursor);*/
// void releaseFunctionBody(Function_body * function_body);

#endif
