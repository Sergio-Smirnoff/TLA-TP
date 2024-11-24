#include "BisonActions.h"

/* MODULE INTERNAL STATE */

static Logger *_logger = NULL;

void initializeBisonActionsModule()
{
	_logger = createLogger("BisonActions");
}

void shutdownBisonActionsModule()
{
	if (_logger != NULL)
	{
		destroyLogger(_logger);
	}
}

/** IMPORTED FUNCTIONS */

extern unsigned int flexCurrentContext(void);

/* PRIVATE FUNCTIONS */

static void _logSyntacticAnalyzerAction(const char *functionName);

/**
 * Logs a syntactic-analyzer action in DEBUGGING level.
 */
static void _logSyntacticAnalyzerAction(const char *functionName)
{
	logDebugging(_logger, "%s", functionName);
}

/* PUBLIC FUNCTIONS */

// new

Program *ProgramSemanticAction(CompilerState *compilerState, Ruleset *ruleset)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Program *program = calloc(1, sizeof(Program));
	program->ruleset = ruleset;
	compilerState->abstractSyntaxTree = program;
	if (0 < flexCurrentContext())
	{
		logError(_logger, "The final context is not the default (0): %d", flexCurrentContext());
		compilerState->succeed = false;
	}
	else
	{
		compilerState->succeed = true;
	}
	return program;
}

// Ruleset
Ruleset *RulesetSemanticAction(Rule *rule, Ruleset *ruleset)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Ruleset *rt = calloc(1, sizeof(Ruleset));
	rt->rule = rule;
	rt->ruleset = ruleset;
	return rt;
}

// Rule
Rule *RuleDefinitionSemanticAction(Lexeme_precursor *lexeme, Action *action, Rule_type type)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Rule *rule = calloc(1, sizeof(Rule));
	rule->lexeme = lexeme;
	rule->action = action;
	rule->type = type;
	return rule;
}

Rule *RuleNewRegexSemanticAction(char *our_regex_id, Regexes *regexes, CompilerState *compilerState)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Rule *rule = calloc(1, sizeof(Rule));
	rule->our_regex_id = our_regex_id;
	rule->regexes = regexes;
	rule->type = REGEX;

	Valid_Regex_List_Node *newNode = calloc(1, sizeof(Valid_Regex_List_Node));
	newNode->regex_id = our_regex_id;
	newNode->next = NULL;

	if (compilerState->validRegexList->head == NULL)
	{
		compilerState->validRegexList->head = newNode;
	}
	else
	{
		Valid_Regex_List_Node *current = compilerState->validRegexList->head;
		while (current->next != NULL)
		{
			current = current->next;
		}
		current->next = newNode;
	}

	compilerState->validRegexList->size++;

	return rule;
}

Lexeme_precursor *LexemePrecursorSemanticAction(Lexeme *lex, Lexeme_precursor *lex_prec, Lexeme_chain_type chain_type)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme_precursor *new_lexeme_precursor = calloc(1, sizeof(Lexeme_precursor));
	new_lexeme_precursor->lexeme = lex;
	new_lexeme_precursor->lexeme_precursor = lex_prec;
	new_lexeme_precursor->precursor_type = NONLITERALS;
	new_lexeme_precursor->chain_type = chain_type;
	return new_lexeme_precursor;
}

Lexeme_precursor *LexemeDefaultSemanticAction()
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme_precursor *new_lexeme_precursor = calloc(1, sizeof(Lexeme_precursor));
	new_lexeme_precursor->precursor_type = DEFAULT_T;
	return new_lexeme_precursor;
}

// Lexeme
Lexeme *LexemeSemanticAction(char *string, Regexes *regex_class, Closure *closure, Lexeme_type type, CompilerState *compilerState)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);

	if (compilerState != NULL)
	{
		Valid_Regex_List_Node *current = compilerState->validRegexList->head;
		unsigned char found = 0;
		while (current != NULL)
		{
			if (strcmp(current->regex_id, string) == 0)
			{
				found = 1;
				break;
			}
			current = current->next;
		}

		if (!found)
		{
			Invalid_Regex_List_Node *newInvalidNode = calloc(1, sizeof(Invalid_Regex_List_Node));
			newInvalidNode->regex_id = string;
			newInvalidNode->next = NULL;

			if (compilerState->invalidRegexList->head == NULL)
			{
				compilerState->invalidRegexList->head = newInvalidNode;
			}
			else
			{
				Invalid_Regex_List_Node *invalidCurrent = compilerState->invalidRegexList->head;
				while (invalidCurrent->next != NULL)
				{
					invalidCurrent = invalidCurrent->next;
				}
				invalidCurrent->next = newInvalidNode;
			}
			compilerState->invalidRegexList->size++;
		}
	}

	Lexeme *lexeme = calloc(1, sizeof(Lexeme));

	switch (type)
	{
	case REGEX:
		lexeme->regexes = regex_class;
		break;
	case NAME:
		lexeme->our_regex_id = string;
		break;
	default:
		compilerState->succeed = false;
		logError(_logger, "Invalid lexeme type: %d", type);
		break;
	}

	lexeme->closure = closure;
	lexeme->type = type;
	return lexeme;
}

Lexeme *LexemeStringSemanticAction(char *string)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme *lexeme = calloc(1, sizeof(Lexeme));
	lexeme->string = string;
	lexeme->type = STRING_LEXEME;
	return lexeme;
}

Lexeme *LexemeClosureSemanticAction(Lexeme_precursor *lex_prec, Closure *closure)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme *new_lexeme = calloc(1, sizeof(Lexeme));
	new_lexeme->type = PRECURSOR_CLOSURE;
	new_lexeme->precursor = lex_prec;
	new_lexeme->closure = closure;
	return new_lexeme;
}

// Closure
Closure *ClosureSemanticAction(Token string)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Closure *closure = calloc(1, sizeof(Closure));
	closure->closure = string;
	return closure;
}

// Regex_class
Regex_class *SymbolRegexSemanticAction(Symbol *sym)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Regex_class *new_regex_class = calloc(1, sizeof(Regex_class));
	new_regex_class->symbol = sym;
	new_regex_class->type = SYMBOL_TYPE;
	return new_regex_class;
}

Regex_class *RegexClassRangeSemanticAction(Symbol *startSymbol, Symbol *endSymbol)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Regex_class *new_regex_class = calloc(1, sizeof(Regex_class));
	new_regex_class->startSymbol = startSymbol;
	new_regex_class->endSymbol = endSymbol;
	new_regex_class->type = RANGE;
	return new_regex_class;
}

Regex_class *CreatedClassSemanticAction(char *string, Closure *closure, CompilerState *compilerState)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);

	Valid_Regex_List_Node *current = compilerState->validRegexList->head;
	unsigned char found = 0;
	while (current != NULL)
	{
		if (strcmp(current->regex_id, string) == 0)
		{
			found = 1;
			break;
		}
		current = current->next;
	}

	if (!found)
	{
		Invalid_Regex_List_Node *newInvalidNode = calloc(1, sizeof(Invalid_Regex_List_Node));
		newInvalidNode->regex_id = string;
		newInvalidNode->next = NULL;

		if (compilerState->invalidRegexList->head == NULL)
		{
			compilerState->invalidRegexList->head = newInvalidNode;
		}
		else
		{
			Invalid_Regex_List_Node *invalidCurrent = compilerState->invalidRegexList->head;
			while (invalidCurrent->next != NULL)
			{
				invalidCurrent = invalidCurrent->next;
			}
			invalidCurrent->next = newInvalidNode;
		}
		compilerState->invalidRegexList->size++;
	}

	Regex_class *new_regex_class = calloc(1, sizeof(Regex_class));
	new_regex_class->varName = string;
	new_regex_class->closure = closure;
	new_regex_class->type = VARIABLE;
	return new_regex_class;
}

// regexes
Regexes *RegexesSemanticAction(Regex_class *regex_class, Regexes *regexes)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Regexes *new_regexes = calloc(1, sizeof(Regexes));
	new_regexes->regexClass = regex_class;
	new_regexes->regexes = regexes;
	return new_regexes;
}

Symbol *RegexSymbolSemanticAction(char *string)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Symbol *symbol = calloc(1, sizeof(Symbol));
	symbol->symbol_tok = string;
	return symbol;
}

// Types
Type *TypeSemanticAction(Token stuff)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Type *type = calloc(1, sizeof(Type));
	type->stuff = stuff;
	return type;
}

// Action
Action *ActionSemanticAction(char *var_name)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Action *new_action = calloc(1, sizeof(Action));
	new_action->varName = var_name;
	new_action->type = ACTION_T;
	return new_action;
}

Action *ActionJavaSemanticAction(Block *body)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Action *action = calloc(1, sizeof(Action));

	action->type = FUNCTION_BODY;
	action->block = body;

	return action;
}

// Java
// NumericComparison
NumericComparison *JavaNumericComparisonSemanticAction(Token token)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	NumericComparison *numericComparison = calloc(1, sizeof(NumericComparison));
	numericComparison->token = token;
	return numericComparison;
}

// Block
Block *JavaBlockSemanticAction(Statement *state, Block *block)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Block *new_block = calloc(1, sizeof(Block));
	new_block->statement = state;
	new_block->block = block;
	new_block->type = STATEMENT;
	return new_block;
}

Block *JavaReturnExpressionSemanticAction(Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Block *block = calloc(1, sizeof(Block));
	block->expression = exp;
	block->type = RET;
	return block;
}

Block *JavaThrowExpressionSemanticAction(Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Block *block = calloc(1, sizeof(Block));
	block->expression = exp;
	block->type = THROW;

	return block;
}

// Statement
Statement *JavaStatementExpressionSemanticAction(StatementExpression *sexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->statement_expression = sexp;
	statement->type = STATE_TYPE;

	return statement;
}

Statement *IfStatementSemanticAction(IfThenStatement *ifs)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->if_then_statement = ifs;
	statement->type = IF_THEN_STATEMENT;

	return statement;
}

Statement *WhileStatementSemanticAction(Expression *exp, Block *block)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->while_expression = exp;
	statement->while_block = block;
	statement->type = WHILE_TYPE;

	return statement;
}

Statement *ForStatementSemanticAction(ForInit *fors, Expression *exp, StatementExpressionList *list, Block *block)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->for_init = fors;
	statement->for_expression = exp;
	statement->statement_expression_list = list;
	statement->for_block = block;
	statement->type = FOR_TYPE;

	return statement;
}

// ForInit
ForInit *ForInitExpressionListSemanticAction(StatementExpressionList *list)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ForInit *forInit = calloc(1, sizeof(ForInit));
	forInit->statement_expression_list = list;
	forInit->for_type = STATEMENT_EXPRESSION_LIST;

	return forInit;
}

ForInit *JavaVarTypeDefinitionSemantictAction(Type *type, char *var_name, ForInitType for_type, CompilerState *compilerState)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ForInit *forInit = calloc(1, sizeof(ForInit));
	switch (for_type)
	{
	case WITH_TYPES:
		forInit->type = type;
		forInit->var_name_type = var_name;
		break;
	case WITHOUT_TYPES:
		forInit->var_name = var_name;
		break;
	default:
		compilerState->succeed = false;
		logError(_logger, "Invalid for init type: %d", for_type);
		break;
	}
	forInit->for_type = for_type;

	return forInit;
}

// StatementExpressionList
StatementExpressionList *StatementExpressionListSemanticAction(StatementExpression *exp, StatementExpressionList *list)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpressionList *statementExpressionList = calloc(1, sizeof(StatementExpressionList));
	statementExpressionList->expression = exp;
	statementExpressionList->expression_list = list;
	return statementExpressionList;
}

// IfThenStatement
IfThenStatement *JavaIfThenStructureSemanticAction(Expression *exp, Block *ifBlock, Block *elseBlock)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfThenStatement *ifThenStatement = calloc(1, sizeof(IfThenStatement));
	ifThenStatement->expression = exp;
	ifThenStatement->if_block = ifBlock;
	ifThenStatement->else_block = elseBlock;
	return ifThenStatement;
}

// StatementExpression
StatementExpression *JavaAsignmentSemanticAction(Assignment *assignment)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpression *statementExpression = calloc(1, sizeof(StatementExpression));
	statementExpression->assignment = assignment;
	statementExpression->state_type = ASSIGNATION;
	return statementExpression;
}

StatementExpression *JavaPostFixAccessDefaultSemanticAction(PostfixExpression * postfix_expression)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpression *statementExpression = calloc(1, sizeof(StatementExpression));
	statementExpression->postfix_expression= postfix_expression;
	statementExpression->state_type = POSTFIX_ACCESS;
	return statementExpression;
}

StatementExpression *JavaAsignmentTypeSemanticAction(Type *type, char *var_name, Token java_assignment, Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpression *statementExpression = calloc(1, sizeof(StatementExpression));
	statementExpression->type = type;
	statementExpression->var_name = var_name;
	statementExpression->expression = exp;
	statementExpression->state_type = ASSIG_TYPE;
	return statementExpression;
}

// VarAccess
VarAccess *VarAccessMethodInvocationSemanticAction(MethodInvocation *method_invocation)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VarAccess *varAccess = calloc(1, sizeof(VarAccess));
	varAccess->method_invocation = method_invocation;

	return varAccess;
}

VarAccess *VarAccessVarSemanticAction(char *var_name)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VarAccess *varAccess = calloc(1, sizeof(VarAccess));
	varAccess->var_name = var_name;

	return varAccess;
}

VarAccess *VarAccessVarOperatorSemanticAction(char *var_name, VarAccess *vaccess)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VarAccess *varAccess = calloc(1, sizeof(VarAccess));
	varAccess->var_name = var_name;
	varAccess->vaccess = vaccess;

	return varAccess;
}

VarAccess *VarAccessTypeOperatorSemanticAction(Type *type, VarAccess *vaccess)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VarAccess *varAccess = calloc(1, sizeof(VarAccess));
	varAccess->type = type;
	varAccess->vaccess = vaccess;

	return varAccess;
}

// MethodInvocation
MethodInvocation *InvocationSemanticAction(VarAccess *vaccess, ArgumentList *arglist)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MethodInvocation *methodInvocation = calloc(1, sizeof(MethodInvocation));
	methodInvocation->vaccess = vaccess;
	methodInvocation->arglist = arglist;
	return methodInvocation;
}

// ArgumentList
ArgumentList *ArgListSemanticExpression(Expression *exp, ArgumentList *arglist)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentList *argumentList = calloc(1, sizeof(ArgumentList));
	argumentList->expression = exp;
	argumentList->arglist = arglist;
	return argumentList;
}

// Expression
Expression *expressionSematicAction(ConditionalExpression *xexpression, Assignment *assig)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression *expression = calloc(1, sizeof(Expression));
	if (xexpression != NULL)
	{
		expression->conditional_expression = xexpression;
		expression->type = CONDITIONAL_EXP;
	}
	else
	{
		expression->assignment = assig;
		expression->type = ASSIGNMENT_TYPE;
	}
	return expression;
}

// ConditionalExpression
ConditionalExpression *JavaConditionalExpSemanticAction(ConditionalOrExpression *corexp, Expression *exp, ConditionalExpression *cexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ConditionalExpression *conditionalExpression = calloc(1, sizeof(ConditionalExpression));
	conditionalExpression->conditional_or_expression = corexp;
	conditionalExpression->expression = exp;
	conditionalExpression->conditional_expression = cexp;
	return conditionalExpression;
}

// ConditionalOrExpression
ConditionalOrExpression *JavaConditionalOrExpressionSemanticAction(ConditionalAndExpression *candexp, ConditionalOrExpression *corexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ConditionalOrExpression *conditionalOrExpression = calloc(1, sizeof(ConditionalOrExpression));
	conditionalOrExpression->conditional_and_expression = candexp;
	conditionalOrExpression->conditional_or_expression = corexp;
	return conditionalOrExpression;
}

// ConditionalAndExpression
ConditionalAndExpression *JavaConditionalAndExpressionSemanticAction(ConditionalAndExpression *andexp, EqualityExpression *eqexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ConditionalAndExpression *conditionalAndExpression = calloc(1, sizeof(ConditionalAndExpression));
	conditionalAndExpression->conditional_and_expression = andexp;
	conditionalAndExpression->equality_expression = eqexp;
	return conditionalAndExpression;
}

// EqualityExpression
EqualityExpression *EqualityExpressionSemanticAction(UnaryExpression *uexp, Token token, EqualityExpression *eqexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	EqualityExpression *equalityExpression = calloc(1, sizeof(EqualityExpression));
	equalityExpression->unary_expression = uexp;
	equalityExpression->equality_expression = eqexp;
	equalityExpression->token = token;
	return equalityExpression;
}

// UnaryExpression
UnaryExpression *UnaryExpressionNumericComparisonSintaticAction(UnaryExpression *uexp1, NumericComparison *numcomp, PostfixExpression *uexp2)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->num_comp_unary_exp1 = uexp1;
	unaryExpression->numcomp = numcomp;
	unaryExpression->num_comp_unary_exp2 = uexp2;
	unaryExpression->globaltype = NUMERIC_COMPARISON;
	return unaryExpression;
}

UnaryExpression *UnaryExpressionDoubleTokenSintaticAction(UnaryExpression *uexp1, UnaryExpressionType type, PostfixExpression *uexp2)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->uexp_unary_expression1 = uexp1;
	unaryExpression->uexp_type = type;
	unaryExpression->uexp_unary_expression2 = uexp2;
	unaryExpression->globaltype = DOUBLE_TOKEN;
	return unaryExpression;
}

UnaryExpression *UnaryExpressionPostfixExpressionSintaticAction(PostfixExpression *pexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->postfix_expression = pexp;
	unaryExpression->globaltype = POSTFIX_EXPRESSION;

	return unaryExpression;
}

UnaryExpression *UnaryExpressionTypeSintaticAction(Type *typ)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->object_type = typ;
	unaryExpression->globaltype = TYPE;
	return unaryExpression;
}

UnaryExpression *UnaryExpressionSingleTokenSintaticAction(UnaryExpression *uexp, Token token)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->unary_expression = uexp;
	unaryExpression->token = token;
	unaryExpression->globaltype = SINGLE_TOKEN;

	return unaryExpression;
}

// PostfixExpression
PostfixExpression *PostfixExpressionPrimarySemanticAction(Primary *primary)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PostfixExpression *postfixExpression = calloc(1, sizeof(PostfixExpression));
	postfixExpression->primary = primary;

	return postfixExpression;
}

PostfixExpression *PostfixExpressionVAccessSemanticAction(VarAccess *vaccess, Token token)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PostfixExpression *postfixExpression = calloc(1, sizeof(PostfixExpression));
	postfixExpression->vaccess = vaccess;
	postfixExpression->token = token;

	return postfixExpression;
}

PostfixExpression *PostfixExpressionVAccessDefaultSemanticAction(VarAccess *vaccess)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PostfixExpression *postfixExpression = calloc(1, sizeof(PostfixExpression));
	postfixExpression->vaccess = vaccess;

	return postfixExpression;
}

// Assignment
Assignment *AssignmentSemanticAction(VarAccess *vaccess, Token token, Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Assignment *assignment = calloc(1, sizeof(Assignment));
	assignment->vaccess = vaccess;
	assignment->expression = exp;
	assignment->token = token;

	return assignment;
}

// Primary
Primary *PrimaryLiteralSemanticAction(Literal *lit)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Primary *primary = calloc(1, sizeof(Primary));
	primary->literal = lit;
	primary->type = LITERAL_TYPE;
	return primary;
}

Primary *PrimaryExpressionSemanticAction(Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Primary *primary = calloc(1, sizeof(Primary));
	primary->expression = exp;
	primary->type = EXPRESSION_TYPE;

	return primary;
}

Primary *PrimaryCExpSemanticAction(ClassInstanceCreationExpression *cice)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Primary *primary = calloc(1, sizeof(Primary));
	primary->class_inst_creation_exp = cice;
	primary->type = CONDITIONAL_EXPRESSION_TYPE;

	return primary;
}

// ClassInstanceCreationExpression
ClassInstanceCreationExpression *InstanceCreationExpressionSemanticAction(UnqualifiedClassInstanceCreationExpression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ClassInstanceCreationExpression *classInstanceCreationExpression = calloc(1, sizeof(ClassInstanceCreationExpression));
	classInstanceCreationExpression->unq_class_inst_creation_exp = exp;
	return classInstanceCreationExpression;
}

ClassInstanceCreationExpression *VAccessInstanceCreationExpressionSemanticAction(VarAccess *vaccess, UnqualifiedClassInstanceCreationExpression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ClassInstanceCreationExpression *classInstanceCreationExpression = calloc(1, sizeof(ClassInstanceCreationExpression));
	classInstanceCreationExpression->vaccess = vaccess;
	classInstanceCreationExpression->unq_class_inst_creation_exp = exp;
	return classInstanceCreationExpression;
}

ClassInstanceCreationExpression *PrimaryInstanceCreationExpressionSemanticAction(Primary *primary, UnqualifiedClassInstanceCreationExpression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ClassInstanceCreationExpression *classInstanceCreationExpression = calloc(1, sizeof(ClassInstanceCreationExpression));
	classInstanceCreationExpression->primary = primary;
	classInstanceCreationExpression->unq_class_inst_creation_exp = exp;
	return classInstanceCreationExpression;
}

// UnqualifiedClassInstanceCreationExpression
UnqualifiedClassInstanceCreationExpression *UnqualifiedClassSemanticAction(Type *type, ArgumentList *list)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnqualifiedClassInstanceCreationExpression *unqualifiedClassInstanceCreationExpression = calloc(1, sizeof(UnqualifiedClassInstanceCreationExpression));
	unqualifiedClassInstanceCreationExpression->type = type;
	unqualifiedClassInstanceCreationExpression->arglist = list;
	unqualifiedClassInstanceCreationExpression->unq_type = PARARGS_TYPE;
	return unqualifiedClassInstanceCreationExpression;
}

UnqualifiedClassInstanceCreationExpression *UnqualifiedClassSemanticActionInvocation(MethodInvocation *invocation)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnqualifiedClassInstanceCreationExpression *unqualifiedClassInstanceCreationExpression = calloc(1, sizeof(UnqualifiedClassInstanceCreationExpression));
	unqualifiedClassInstanceCreationExpression->invocation = invocation;
	unqualifiedClassInstanceCreationExpression->unq_type = METHOD_TYPE;
	return unqualifiedClassInstanceCreationExpression;
}

// Literal
Literal *JavaLiteralStrSemanticAction(char *string)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Literal *literal = calloc(1, sizeof(Literal));
	literal->str = string;
	literal->type = STRING_T;
	return literal;
};

Literal *JavaLiteralTokenSemanticAction(Token tok)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Literal *literal = calloc(1, sizeof(Literal));
	literal->token = tok;
	literal->type = TOKEN_T;
	return literal;
};
