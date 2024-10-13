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
typedef struct Regex_class Regex_class;
typedef struct Java_function_body Java_function_body;

typedef struct NumericComparison NumericComparison;
typedef struct Statement Statement;
typedef struct ForInit ForInit;
typedef struct StatementExpressionList StatementExpressionList;
typedef struct IfThenStatement IfThenStatement;
typedef struct IfThenElseStatement IfThenElseStatement;
typedef struct IfThenElseStatementNoShortIf IfThenElseStatementNoShortIf;
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
typedef struct UnaryExpressionNotPlusMinus UnaryExpressionNotPlusMinus;
typedef struct PostfixExpression PostfixExpression;
typedef struct Assignment Assignment;
typedef struct Primary Primary;
typedef struct PrimaryNoNewArray PrimaryNoNewArray;
typedef struct ClassInstanceCreationExpression ClassInstanceCreationExpression;
typedef struct UnqualifiedClassInstanceCreationExpression UnqualifiedClassInstanceCreationExpression;
typedef struct Literal Literal;
typedef struct ArrayCreationExpression ArrayCreationExpression;
typedef struct Dims Dims;
/**
 * Node types for the Abstract Syntax Tree (AST).
 */


typedef enum NumericComparison_type{
	TODO_1
}NumericComparison_type;

struct NumericComparison{
	char * TODO;
	NumericComparison_type;
};

typedef enum Statement_type{
	TODO_2
}Statement_type;

struct Statement{
	char * TODO;
	Statement_type;
};

typedef enum ForInit_type{
	TODO_3
}ForInit_type;

struct ForInit{
	char * TODO;
	ForInit_type;
};

typedef enum StatementExpressionList_type{
	TODO_4
}StatementExpressionList_type;

struct StatementExpressionList{
	char * TODO;
	StatementExpressionList_type;
};

typedef enum IfThenStatement_type{
	TODO_5
}IfThenStatement_type;

struct IfThenStatement{
	char * TODO;
	IfThenStatement_type;
};

typedef enum IfThenElseStatement_type{
	TODO_6
}IfThenElseStatement_type;

struct IfThenElseStatement{
	char * TODO;
	IfThenElseStatement_type;
};

typedef enum IfThenElseStatementNoShortIf_type{
	TODO_7
}IfThenElseStatementNoShortIf_type;

struct IfThenElseStatementNoShortIf{
	char * TODO;
	IfThenElseStatementNoShortIf_type;
};

typedef enum StatementWithoutTrailingSubstatement_type{
	TODO_8
}StatementWithoutTrailingSubstatement_type;

struct StatementWithoutTrailingSubstatement{
	char * TODO;
	StatementWithoutTrailingSubstatement_type;
};

typedef enum StatementExpression_type{
	TODO_9
}StatementExpression_type;

struct StatementExpression{
	char * TODO;
	StatementExpression_type;
};

typedef enum VarAccess_type{
	TODO_10
}VarAccess_type;

struct VarAccess{
	char * TODO;
	VarAccess_type;
};

typedef enum MethodInvocation_type{
	TODO_11
}MethodInvocation_type;

struct MethodInvocation{
	char * TODO;
	MethodInvocation_type;
};

typedef enum ArgumentList_type{
	TODO_12
}ArgumentList_type;

struct ArgumentList{
	char * TODO;
	ArgumentList_type;
};

typedef enum Expression_type{
	TODO_13
}Expression_type;

struct Expression{
	char * TODO;
	Expression_type;
};

typedef enum ConditionalExpression_type{
	TODO_14
}ConditionalExpression_type;

struct ConditionalExpression{
	char * TODO;
	ConditionalExpression_type;
};

typedef enum ConditionalOrExpression_type{
	TODO_15
}ConditionalOrExpression_type;

struct ConditionalOrExpression{
	char * TODO;
	ConditionalOrExpression_type;
};

typedef enum ConditionalAndExpression_type{
	TODO_16
}ConditionalAndExpression_type;

struct ConditionalAndExpression{
	char * TODO;
	ConditionalAndExpression_type;
};

typedef enum EqualityExpression_type{
	TODO_17
}EqualityExpression_type;

struct EqualityExpression{
	char * TODO;
	EqualityExpression_type;
};

typedef enum RelationalExpression_type{
	TODO_18
}RelationalExpression_type;

struct RelationalExpression{
	char * TODO;
	RelationalExpression_type;
};

typedef enum AdditiveExpression_type{
	TODO_19
}AdditiveExpression_type;

struct AdditiveExpression{
	char * TODO;
	AdditiveExpression_type;
};

typedef enum MultiplicativeExpression_type{
	TODO_20
}MultiplicativeExpression_type;

struct MultiplicativeExpression{
	char * TODO;
	MultiplicativeExpression_type;
};

typedef enum UnaryExpression_type{
	TODO_21
}UnaryExpression_type;

struct UnaryExpression{
	char * TODO;
	UnaryExpression_type;
};

typedef enum UnaryExpressionNotPlusMinus_type{
	TODO_22
}UnaryExpressionNotPlusMinus_type;

struct UnaryExpressionNotPlusMinus{
	char * TODO;
	UnaryExpressionNotPlusMinus_type;
};

typedef enum PostfixExpression_type{
	TODO_23
}PostfixExpression_type;

struct PostfixExpression{
	char * TODO;
	PostfixExpression_type;
};

typedef enum Assignment_type{
	TODO_24
}Assignment_type;

struct Assignment{
	char * TODO;
	Assignment_type;
};

typedef enum Primary_type{
	TODO_25
}Primary_type;

struct Primary{
	char * TODO;
	Primary_type;
};

typedef enum PrimaryNoNewArray_type{
	TODO_26
}PrimaryNoNewArray_type;

struct PrimaryNoNewArray{
	char * TODO;
	PrimaryNoNewArray_type;
};

typedef enum ClassInstanceCreationExpression_type{
	TODO_27
}ClassInstanceCreationExpression_type;

struct ClassInstanceCreationExpression{
	char * TODO;
	ClassInstanceCreationExpression_type;
};

typedef enum UnqualifiedClassInstanceCreationExpression_type{
	TODO_28
}UnqualifiedClassInstanceCreationExpression_type;

struct UnqualifiedClassInstanceCreationExpression{
	char * TODO;
	UnqualifiedClassInstanceCreationExpression_type;
};

typedef enum Literal_type{
	TODO_29
}Literal_type;

struct Literal{
	char * TODO;
	Literal_type;
};

typedef enum ArrayCreationExpression_type{
	TODO_30
}ArrayCreationExpression_type;

struct ArrayCreationExpression{
	char * TODO;
	ArrayCreationExpression_type;
};

typedef enum Dims_type{
	TODO_31
}Dims_type;

struct Dims{
	char * TODO;
	Dims_type;
};

struct Function_body{
	char* log;
	char* ret;
};

struct Param{
	Token stuff;
};

struct Closure{
	Token closure;
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
		Lexeme* lexeme;
		struct{
			Lexeme* lex;
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
//void releaseFunctionBody(Function_body * function_body);


#endif


