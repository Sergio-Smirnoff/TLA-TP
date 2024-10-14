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


struct NumericComparison{
	Token token;
};

typedef enum Block_type{
	statement,
	throw,
	ret
} Block_type;

struct Block{
	union
	{
		struct{
			Statement * statement;
			Block * block;
		};
		Expression * exp;
	};
	Block_type type;
};

typedef enum Statement_type{
	state,
	ifThenStatement,
	While,
	For
}Statement_type;

struct Statement{
	union{
		StatementExpression * sexp;
		IfThenStatement * ifThen;
		struct{
			Expression *expwhile;
			Statement *statementwhile;
		};
		struct{
			ForInit *forInit;
			Expression *expfor;
			Statement *statementfor;
			StatementExpressionList *statementExpList;
		};
	};
	Statement_type type;
};

typedef enum ForInitType{
	statementExpList,
	withParams,
	withoutParams
} ForInitType;

struct ForInit{
	union{
		StatementExpressionList* statementExpList;
		struct{
			Param* param;
			char* var_name_param;
		};
		char * var_name;
	};
	ForInitType type;

};


struct StatementExpressionList{
	StatementExpression* exp;
	StatementExpressionList* list;
};


struct IfThenStatement{
	Expression* exp;
	Statement* statement1;
	Statement* statement2;
};

typedef enum StatementExpressionType{
	assignation,
	invocation,
	assigParam,
}StatementExpressionType;


struct StatementExpression{
	union{
		Assignment* assignment;
		MethodInvocation* method_invocation;
		struct{
			Param* param;
			char* var_name;
			Expression* exp;
		};
	};
	StatementExpressionType type;
};


struct VarAccess{
	char* var_name;
	VarAccess* vaccess;
	Token token;
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
	ConditionalAndExpression* candexp;
};


struct EqualityExpression{
	UnaryExpression* uexp;
	EqualityExpression* eqexp;
};

typedef enum GlobalUnaryExpressionType{
	numericComparison,
	doubleToken,
	postfixExpression,
	param,
	singleToken
} GlobalUnaryExpressionType;

typedef enum UnaryExpressionType{
	star_t,
	div_type,
	mod_t,
	plus_t,
	minus_t
} UnaryExpressionType;


struct UnaryExpression{
	union{
		struct{
			UnaryExpression* uexp1_num;
			NumericComparison* numcomp;
			UnaryExpression* uexp2_num;
		};
		struct{
			UnaryExpression* uexp1_exp;
			UnaryExpressionType type;
			UnaryExpression* uexp2_exp;
		};
		PostfixExpression* pexp;
		Param* param;
		struct{
			Token token;
			UnaryExpression* uexp;
		};
	};
	GlobalUnaryExpressionType globaltype;
};


struct PostfixExpression{
	Primary* primary;
	VarAccess* vaccess;
	Token token;
};


struct Assignment{
	VarAccess * vaccess;
	Expression * expression;
};

typedef enum PrimaryType{
	literal,
	expression,
	cexp
} PrimaryType;


struct Primary{
	union{
		Literal* lit;
		Expression* exp;
		ClassInstanceCreationExpression* cice;
	};
	PrimaryType type;
};


struct ClassInstanceCreationExpression{
	UnqualifiedClassInstanceCreationExpression* ucice;
	VarAccess* vaccess;
	Primary* primary;
};


struct UnqualifiedClassInstanceCreationExpression{
	Param* param;
	ArgumentList* arglist;
};

typedef enum Literal_type{
	str,
	token
} Literal_type;

struct Literal{
	union{
		char* str;
		Token token;
	};
	Literal_type type;
};

struct Function_body{
	char* log;
	char* ret;
};

typedef enum Lexeme_type {
	regexes,
	name,
	default_lexeme,
	string_lexeme
} Lexeme_type;

struct Lexeme{
	union{
		struct{
			Regexes* regexes;
			Closure* closure;
		};
		struct{
			char* our_regex_id;
			Closure* clre;
		};
	};
	Lexeme_type type;
};

typedef enum Lexeme_precursor_type{
	literals,
	nonliterals
} Lexeme_precursor_type;

struct Lexeme_precursor{
	union {
		struct {
			Lexeme *lex;
			Lexeme_precursor *lex_prec;
		};
		struct {
			char* string;
			Lexeme_type type;
		};
	};
	Lexeme_precursor_type precursor_type;
};

struct Param{
	Token stuff;
};

struct Closure{
	Token closure;
};

struct Regexes{
	Regex_class *regexClass;
	Regexes *regexes;
};

struct Symbol{
	char* symbol_tok;
};

typedef enum Regex_class_type{
	symbol,
	range,
	variable
} Regex_class_type;

typedef struct Regex_class {
	union{
		struct{
			Symbol* startSymbol;
			Symbol* endSymbol;
		};
		struct{
			char* varName;
			Closure* closure;
		};
		Symbol *symbol;
	};
	Regex_class_type type;
} Regex_class;

typedef enum Action_type{
	action,
	function_body
} Action_type;

struct Action {
	union{
		char* varName;
		struct{
			Param* param;
			Block *block;
		};
	};
	Action_type type;
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
			Regexes* regexes;
		};
		Lexeme_precursor* lexeme;
		struct{
			Lexeme_precursor* lex;
			Action* action;
		};
	};
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
/*
void releaseProgram(Program * program);
void releaseRuleset(Ruleset * ruleset);
void releaseRule(Rule * rule);
void releaseLexeme(Lexeme * lexeme);
void releaseAction(Action * action);
void releaseRegexClass(Regex_class * regex_class);
void releaseClosure(Closure * closure);
void releaseParam(Param * param);
void releaseLexemePrecursor(Lexeme_precursor * lexeme_precursor);*/
//void releaseFunctionBody(Function_body * function_body);


#endif


