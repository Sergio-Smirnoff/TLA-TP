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
struct Program
{
	Ruleset *ruleset;
};

struct Ruleset
{
	Rule *rule;
	Ruleset *ruleset;
};

typedef enum Rule_type
{
	REGEX,
	IGNORE_LEXEME,
	LEXEME_ACTION
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

// Regexes
struct Regexes
{
	Regex_class *regexClass;
	Regexes *regexes;
};

typedef enum Regex_class_type
{
	SYMBOL_TYPE,
	RANGE,
	VARIABLE
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

// Lexemes
typedef enum Lexeme_precursor_type
{
	DEFAULT_T,
	NONLITERALS
} Lexeme_precursor_type;

typedef enum Lexeme_chain_type
{
	END,
	CONCATENATION,
	SUMMATION
} Lexeme_chain_type;

struct Lexeme_precursor
{
	Lexeme *lexeme;
	Lexeme_precursor *lexeme_precursor;
	Lexeme_precursor_type precursor_type;
	Lexeme_chain_type chain_type;
};

typedef enum Lexeme_type
{
	REGEXES_TYPE,
	NAME,
	STRING_LEXEME,
	PRECURSOR_CLOSURE
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

// Multipurpose (Regexes and Lexemes)
struct Symbol
{
	char *symbol_tok;
};

struct Closure
{
	Token closure;
};

// Actions
typedef enum Action_type
{
	ACTION_T,
	FUNCTION_BODY
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

// Java
typedef enum Block_type
{
	STATEMENT,
	THROW,
	RET
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
		Expression *expression;
	};
	Block_type type;
};

// Statements
typedef enum Statement_type
{
	STATE_TYPE,
	IF_THEN_STATEMENT,
	WHILE_TYPE,
	FOR_TYPE
} Statement_type;

struct Statement
{
	union
	{
		StatementExpression *statement_expression;
		IfThenStatement *if_then_statement;
		struct
		{
			Expression *while_expression;
			Block *while_block;
		};
		struct
		{
			ForInit *for_init;
			Expression *for_expression;
			Block *for_block;
			StatementExpressionList *statement_expression_list;
		};
	};
	Statement_type type;
};

typedef enum StatementExpressionType
{
	ASSIGNATION,
	UNARY_ACCESS,
	ASSIG_TYPE,
} StatementExpressionType;

struct StatementExpression
{
	union
	{
		Assignment *assignment;
		UnaryExpression *unary_expression;
		struct
		{
			Type *type;
			char *var_name;
			Expression *expression;
		};
	};
	StatementExpressionType state_type;
};

struct IfThenStatement
{
	Expression *expression;
	Block *if_block;
	Block *else_block;
};

typedef enum ForInitType
{
	STATEMENT_EXPRESSION_LIST,
	WITH_TYPES,
	WITHOUT_TYPES
} ForInitType;

struct ForInit
{
	union
	{
		StatementExpressionList *statement_expression_list;
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
	StatementExpression *expression;
	StatementExpressionList *expression_list;
};

// Expressions
typedef enum Expression_type
{
	CONDITIONAL_EXP,
	ASSIGNMENT_TYPE
} Expression_type;

struct Expression
{
	union
	{
		ConditionalExpression *conditional_expression;
		Assignment *assignment;
	};
	Expression_type type;
};

struct ConditionalExpression
{
	ConditionalOrExpression *conditional_or_expression;
	Expression *expression;
	ConditionalExpression *conditional_expression;
};

struct ConditionalOrExpression
{
	ConditionalAndExpression *conditional_and_expression;
	ConditionalOrExpression *conditional_or_expression;
};

struct ConditionalAndExpression
{
	EqualityExpression *equality_expression;
	ConditionalAndExpression *conditional_and_expression;
};

struct EqualityExpression
{
	UnaryExpression *unary_expression;
	EqualityExpression *equality_expression;
	Token token;
};

typedef enum GlobalUnaryExpressionType
{
	NUMERIC_COMPARISON,
	DOUBLE_TOKEN,
	POSTFIX_EXPRESSION,
	TYPE,
	SINGLE_TOKEN
} GlobalUnaryExpressionType;

typedef enum UnaryExpressionType
{
	STAR_TYPE,
	DIV_TYPE,
	MOD_TYPE,
	PLUS_TYPE,
	MINUS_TYPE
} UnaryExpressionType;

struct UnaryExpression
{
	union
	{
		struct
		{
			UnaryExpression *num_comp_unary_exp1;
			NumericComparison *numcomp;
			PostfixExpression *num_comp_unary_exp2;
		};
		struct
		{
			UnaryExpression *uexp_unary_expression1;
			UnaryExpressionType uexp_type;
			PostfixExpression *uexp_unary_expression2;
		};
		PostfixExpression *postfix_expression;
		Type *object_type;
		struct
		{
			Token token;
			UnaryExpression *unary_expression;
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

typedef enum UnqualifiedClassInstanceCreationExpression_type
{
	METHOD_TYPE,
	PARARGS_TYPE
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

struct ClassInstanceCreationExpression
{
	UnqualifiedClassInstanceCreationExpression *unq_class_inst_creation_exp;
	VarAccess *vaccess;
	Primary *primary;
};

typedef enum PrimaryType
{
	LITERAL_TYPE,
	EXPRESSION_TYPE,
	CONDITIONAL_EXPRESSION_TYPE
} PrimaryType;

struct Primary
{
	union
	{
		Literal *literal;
		Expression *expression;
		ClassInstanceCreationExpression *class_inst_creation_exp;
	};
	PrimaryType type;
};

typedef enum Literal_type
{
	STRING_T,
	TOKEN_T
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

// Assignment
struct Assignment
{
	VarAccess *vaccess;
	Expression *expression;
	Token token;
};

// Other stuff
struct NumericComparison
{
	Token token;
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

struct Function_body
{
	char *log;
	char *ret;
};

struct Type
{
	Token stuff;
};

/**
 * Node recursive destructors.
 */
void releaseProgram(Program *program);

#endif
