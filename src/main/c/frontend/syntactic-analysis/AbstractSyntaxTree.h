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
typedef struct IfThenElseStatement IfThenElseStatement;
typedef struct StatementWithoutTrailingSubstatement StatementWithoutTrailingSubstatement;
typedef struct StatementExpression StatementExpression;
typedef struct VarAccess VarAccess;
typedef struct MethodInvocation MethodInvocation;
typedef struct ArgumentList ArgumentList;
typedef struct Expression Expression;
typedef struct ConditionalExpression ConditionalExpression;
typedef struct ConditionalOrExpression ConditionalOrExpression;
typedef struct ConditionalAndExpression ConditionalAndExpression;
typedef struct EqualityExpression EqualityExpression;
typedef struct RelationalExpression RelationalExpression;
typedef struct AdditiveExpression AdditiveExpression;
typedef struct MultiplicativeExpression MultiplicativeExpression;
typedef struct UnaryExpression UnaryExpression;
typedef struct BinaryExpression BinaryExpression;
typedef struct UnaryExpressionNotPlusMinus UnaryExpressionNotPlusMinus;
typedef struct PostfixExpression PostfixExpression;
typedef struct Assignment Assignment;
typedef struct Primary Primary;
typedef struct ClassInstanceCreationExpression ClassInstanceCreationExpression;
typedef struct UnqualifiedClassInstanceCreationExpression UnqualifiedClassInstanceCreationExpression;
typedef struct Literal Literal;

/**
 * Node types for the Abstract Syntax Tree (AST).
 */



struct NumericComparison{
	Token token;
};

struct Block{
	Statement * statement;
	Block * block;
};

struct Statement{
	StatementWithoutTrailingSubstatement* inline; 
	IfThenStatement* ifThen; 
	ifThenElseStatement* ifelse;
};

typedef enum ForInitType{
	statelist,
	withparams
} ForInitType;

struct ForInit{
	union{
		StatementExpresionList* sel;
		struct{
			Param* param;
			char* var_name;
		};
	};
	ForInitType type;

};


struct StatementExpressionList{
	StatementExpression* exp;
	StatementExpressionList* list;
};


struct IfThenStatement{
	Expression* exp;
	Statement* state;
};


struct IfThenElseStatement{
	Expression* exp;
	Statement* ifThen;
	Statement* elseThen;
};

typedef enum StatementExpressionType{
	sexp,
	exp
}StatementExpressionType;

struct StatementWithoutTrailingSubstatement{
	union{
		StatementExpression* sexp;
		Expression* expression;
	};
	StatementExpressionType type;
};

typedef enum StatementExpressionType{
	post,
	pre
}StatementExpressionType;


struct StatementExpression{
	union{
		Assignment* assignment;
		struct{
			StatementExpressionType type;
			Token token;
			UnaryExpression* exp;
		};
		MethodInvocation* method_invocation;
		struct{
			Param* param;
			char* var_name;
			Token java_assignment;
			Expression* exp;
		};
	};
	StatementExpressionType type;
};


struct VarAccess{
	char* var_name;
	VarAccess* vaccess;
	Param* param;
	MethodInvocation* method_invocation;
};


struct MethodInvocation{
	VarAccess* vaccess;
	ArgumentList* arglist;
};


struct ArgumentList{
	Expression* expression;
	ArgumentList* arglist;
};

typedef enum Expression_type{
	xexp,
	assignment
} Expression_type;

struct Expression{
	union{
		ConditionalExpression* xexp;
		Assignment* assignment;
	};
	Expression_type type;
};


struct ConditionalExpression{
	ConditionalOrExpression* corexp;
	Expression* exp;
	ConditionalExpression* cexp;
};


struct ConditionalOrExpression{
	ConditionalAndExpression* candexp;
	ConditionalOrExpression* corexp;
};


struct ConditionalAndExpression{
	EqualityExpression* eqexp;
	
};


struct EqualityExpression{
	char * TODO;
};


struct RelationalExpression{
	char * TODO;
};


struct AdditiveExpression{
	char * TODO;
};


struct MultiplicativeExpression{
	char * TODO;
};


struct UnaryExpression{
	char * TODO;
};


struct BinaryExpression{
	char * TODO;
};

struct UnaryExpressionNotPlusMinus{
	char * TODO;
};


struct PostfixExpression{
	char * TODO;
};


struct Assignment{
	char * TODO;
};


struct Primary{
	char * TODO;
};


struct ClassInstanceCreationExpression{
	char * TODO;
};


struct UnqualifiedClassInstanceCreationExpression{
	char * TODO;
};


struct Literal{
	char * TODO;
};

struct Function_body{
	char* log;
	char* ret;
};

struct Lexeme_precursor{
	Lexeme *lex;
	Lexeme_precursor *lex_prec;
};

struct Param{
	Token stuff;
};

struct Closure{
	Token closure;
};

struct Regexes{
	char *TODO;
};
struct Symbol{
	char *TODO;
};

typedef enum Regex_class_type{
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

typedef enum Range_type{
	lowercase,
	uppercase,
	number,
	both
} Range_type;

struct Range {
    char* left;
    char* right; 
    Range_type type;
};

typedef enum Action_type{
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

typedef enum java_function_body_type{
	TODO
} java_function_body_type;

struct Java_function_body {
	char * TODO;
	java_function_body_type type;
};

typedef enum Lexeme_type {
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
		Token def;
	};
	Lexeme_type type;
};

typedef enum Rule_type {
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
		Lexeme_precursor* lexeme;
		struct{
			Lexeme_precursor* lex;
			Action* action;
		};

	};
	Token endline;
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
void releaseLexemePrecursor(Lexeme_precursor * lexeme_precursor);
//void releaseFunctionBody(Function_body * function_body);


#endif


