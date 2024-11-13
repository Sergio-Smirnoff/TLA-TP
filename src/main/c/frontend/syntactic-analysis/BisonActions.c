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
	rule->type = regex;

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

// Lexeme precursor
Lexeme_precursor *LexemePrecursorSemanticAction(Lexeme *lex, Lexeme_precursor *lex_prec)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme_precursor *new_lexeme_precursor = calloc(1, sizeof(Lexeme_precursor));
	new_lexeme_precursor->lex = lex;
	new_lexeme_precursor->lex_prec = lex_prec;
	new_lexeme_precursor->precursor_type = nonliterals;
	return new_lexeme_precursor;
}

Lexeme_precursor *LexemeDefaultSemanticAction()
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme_precursor *new_lexeme_precursor = calloc(1, sizeof(Lexeme_precursor));
	new_lexeme_precursor->precursor_type = default_t;
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
	case regex:
		lexeme->regexes = regex_class;
		break;
	case name:
		lexeme->our_regex_id = string;
		break;
	}

	lexeme->closure = closure;
	lexeme->type = type;
	return lexeme;
}

Lexeme *LexemeStringSemanticAction(char *string){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme *lexeme = calloc(1, sizeof(Lexeme));
	lexeme->string = string;
	lexeme->type=string_lexeme;
	return lexeme;
}

Lexeme *LexemeClosureSemanticAction(Lexeme_precursor *lex_prec, Closure *closure)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme *new_lexeme = calloc(1, sizeof(Lexeme));
	new_lexeme->type = precursor_closure;
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
	new_regex_class->type = symbol;
	return new_regex_class;
}

Regex_class *RegexClassRangeSemanticAction(Symbol *startSymbol, Symbol *endSymbol)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Regex_class *new_regex_class = calloc(1, sizeof(Regex_class));
	new_regex_class->startSymbol = startSymbol;
	new_regex_class->endSymbol = endSymbol;
	new_regex_class->type = range;
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
	new_regex_class->type = variable;
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
	new_action->type = action;
	return new_action;
}

Action *ActionJavaSemanticAction(Block *body)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Action *action = calloc(1, sizeof(Action));
	
	action->type = function_body;
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
	new_block->type = statement;
	return new_block;
}

Block *JavaReturnExpressionSemanticAction(Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Block *block = calloc(1, sizeof(Block));
	block->exp = exp;
	block->type = ret;
	return block;
}

Block *JavaThrowExpressionSemanticAction(Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Block *block = calloc(1, sizeof(Block));
	block->exp = exp;
	block->type = throw;

	return block;
}

// Statement
Statement *JavaStatementExpressionSemanticAction(StatementExpression *sexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->sexp = sexp;
	statement->type = state;

	return statement;
}

Statement *IfStatementSemanticAction(IfThenStatement *ifs)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->ifThen = ifs;
	statement->type = ifThenStatement;

	return statement;
}

Statement *WhileStatementSemanticAction(Expression *exp, Block *block)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->expwhile = exp;
	statement->blockwhile = block;
	statement->type = While;

	return statement;
}

Statement *ForStatementSemanticAction(ForInit *fors, Expression *exp, StatementExpressionList *list, Block *block)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->forInit = fors;
	statement->expfor = exp;
	statement->statementExpList = list;
	statement->blockfor = block;
	statement->type = For;

	return statement;
}

// ForInit
ForInit *ForInitExpressionListSemanticAction(StatementExpressionList *list)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ForInit *forInit = calloc(1, sizeof(ForInit));
	forInit->statementExpList = list;
	forInit->for_type = statementExpList;

	return forInit;
}

ForInit *JavaVarTypeDefinitionSemantictAction(Type *type, char *var_name, ForInitType for_type)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ForInit *forInit = calloc(1, sizeof(ForInit));
	switch (for_type)
	{
	case withTypes:
		forInit->type = type;
		forInit->var_name_type = var_name;
		break;
	case withoutTypes:
		forInit->var_name = var_name;
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
	statementExpressionList->exp = exp;
	statementExpressionList->list = list;
	return statementExpressionList;
}

// IfThenStatement
IfThenStatement *JavaIfThenStructureSemanticAction(Expression *exp, Block *ifBlock, Block *elseBlock)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfThenStatement *ifThenStatement = calloc(1, sizeof(IfThenStatement));
	ifThenStatement->exp = exp;
	ifThenStatement->ifblock = ifBlock;
	ifThenStatement->elseblock = elseBlock;
	return ifThenStatement;
}

// StatementExpression
StatementExpression *JavaAsignmentSemanticAction(Assignment *assignment)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpression *statementExpression = calloc(1, sizeof(StatementExpression));
	statementExpression->assignment = assignment;
	statementExpression->state_type = assignation;
	return statementExpression;
}

StatementExpression *JavaVAccessDefaultSemanticAction(VarAccess *var_access)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpression *statementExpression = calloc(1, sizeof(StatementExpression));
	statementExpression->var_access = var_access;
	statementExpression->state_type = vaccess;
	return statementExpression;
}

StatementExpression *JavaAsignmentTypeSemanticAction(Type *type, char *var_name, Token java_assignment, Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementExpression *statementExpression = calloc(1, sizeof(StatementExpression));
	statementExpression->type = type;
	statementExpression->var_name = var_name;
	statementExpression->exp = exp;
	statementExpression->state_type = assigType;
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
		expression->xexp = xexpression;
		expression->type = xexp;
	}
	else
	{
		expression->assignment = assig;
		expression->type = assignment;
	}
	return expression;
}

// ConditionalExpression
ConditionalExpression *JavaConditionalExpSemanticAction(ConditionalOrExpression *corexp, Expression *exp, ConditionalExpression *cexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ConditionalExpression *conditionalExpression = calloc(1, sizeof(ConditionalExpression));
	conditionalExpression->corexp = corexp;
	conditionalExpression->exp = exp;
	conditionalExpression->cexp = cexp;
	return conditionalExpression;
}

// ConditionalOrExpression
ConditionalOrExpression *JavaConditionalOrExpressionSemanticAction(ConditionalAndExpression *candexp, ConditionalOrExpression *corexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ConditionalOrExpression *conditionalOrExpression = calloc(1, sizeof(ConditionalOrExpression));
	conditionalOrExpression->candexp = candexp;
	conditionalOrExpression->corexp = corexp;
	return conditionalOrExpression;
}

// ConditionalAndExpression
ConditionalAndExpression *JavaConditionalAndExpressionSemanticAction(ConditionalAndExpression *andexp, EqualityExpression *eqexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ConditionalAndExpression *conditionalAndExpression = calloc(1, sizeof(ConditionalAndExpression));
	conditionalAndExpression->candexp = andexp;
	conditionalAndExpression->eqexp = eqexp;
	return conditionalAndExpression;
}

// EqualityExpression
EqualityExpression *EqualityExpressionSemanticAction(UnaryExpression *uexp, EqualityExpression *eqexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	EqualityExpression *equalityExpression = calloc(1, sizeof(EqualityExpression));
	equalityExpression->uexp = uexp;
	equalityExpression->eqexp = eqexp;
	return equalityExpression;
}

// UnaryExpression
UnaryExpression *UnaryExpressionNumericComparisonSintaticAction(UnaryExpression *uexp1, NumericComparison *numcomp, PostfixExpression *uexp2)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->uexp1_num = uexp1;
	unaryExpression->numcomp = numcomp;
	unaryExpression->uexp2_num = uexp2;
	unaryExpression->globaltype = numericComparison;
	return unaryExpression;
}

UnaryExpression *UnaryExpressionDoubleTokenSintaticAction(UnaryExpression *uexp1, UnaryExpressionType type, PostfixExpression *uexp2)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->uexp1_exp = uexp1;
	unaryExpression->type = type;
	unaryExpression->uexp2_exp = uexp2;
	unaryExpression->globaltype = doubleToken;
	return unaryExpression;
}

UnaryExpression *UnaryExpressionPostfixExpressionSintaticAction(PostfixExpression *pexp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->pexp = pexp;
	unaryExpression->globaltype = postfixExpression;

	return unaryExpression;
}

UnaryExpression *UnaryExpressionTypeSintaticAction(Type *typ)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->obj_type = typ;
	unaryExpression->globaltype = type;
	return unaryExpression;
}

UnaryExpression *UnaryExpressionSingleTokenSintaticAction(UnaryExpression *uexp, Token token)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryExpression *unaryExpression = calloc(1, sizeof(UnaryExpression));
	unaryExpression->uexp = uexp;
	unaryExpression->token = token;
	unaryExpression->globaltype = singleToken;

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
	primary->lit = lit;
	primary->type = literal;
	return primary;
}

Primary *PrimaryExpressionSemanticAction(Expression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Primary *primary = calloc(1, sizeof(Primary));
	primary->exp = exp;
	primary->type = expression;

	return primary;
}

Primary *PrimaryCExpSemanticAction(ClassInstanceCreationExpression *cice)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Primary *primary = calloc(1, sizeof(Primary));
	primary->cice = cice;
	primary->type = cexp;

	return primary;
}

// ClassInstanceCreationExpression
ClassInstanceCreationExpression *InstanceCreationExpressionSemanticAction(UnqualifiedClassInstanceCreationExpression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ClassInstanceCreationExpression *classInstanceCreationExpression = calloc(1, sizeof(ClassInstanceCreationExpression));
	classInstanceCreationExpression->ucice = exp;
	return classInstanceCreationExpression;
}

ClassInstanceCreationExpression *VAccessInstanceCreationExpressionSemanticAction(VarAccess *vaccess, UnqualifiedClassInstanceCreationExpression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ClassInstanceCreationExpression *classInstanceCreationExpression = calloc(1, sizeof(ClassInstanceCreationExpression));
	classInstanceCreationExpression->vaccess = vaccess;
	classInstanceCreationExpression->ucice = exp;
	return classInstanceCreationExpression;
}

ClassInstanceCreationExpression *PrimaryInstanceCreationExpressionSemanticAction(Primary *primary, UnqualifiedClassInstanceCreationExpression *exp)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ClassInstanceCreationExpression *classInstanceCreationExpression = calloc(1, sizeof(ClassInstanceCreationExpression));
	classInstanceCreationExpression->primary = primary;
	classInstanceCreationExpression->ucice = exp;
	return classInstanceCreationExpression;
}

// UnqualifiedClassInstanceCreationExpression
UnqualifiedClassInstanceCreationExpression *UnqualifiedClassSemanticAction(Type *type, ArgumentList *list)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnqualifiedClassInstanceCreationExpression *unqualifiedClassInstanceCreationExpression = calloc(1, sizeof(UnqualifiedClassInstanceCreationExpression));
	unqualifiedClassInstanceCreationExpression->type = type;
	unqualifiedClassInstanceCreationExpression->arglist = list;
	unqualifiedClassInstanceCreationExpression->unq_type = parargs;
	return unqualifiedClassInstanceCreationExpression;
}

UnqualifiedClassInstanceCreationExpression *UnqualifiedClassSemanticActionInvocation(MethodInvocation *invocation)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnqualifiedClassInstanceCreationExpression *unqualifiedClassInstanceCreationExpression = calloc(1, sizeof(UnqualifiedClassInstanceCreationExpression));
	unqualifiedClassInstanceCreationExpression->invocation = invocation;
	unqualifiedClassInstanceCreationExpression->unq_type = method;
	return unqualifiedClassInstanceCreationExpression;
}

// Literal
Literal *JavaLiteralStrSemanticAction(char *string)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Literal *literal = calloc(1, sizeof(Literal));
	literal->str = string;
	literal->type = str;
	return literal;
};

Literal *JavaLiteralTokenSemanticAction(Token tok)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Literal *literal = calloc(1, sizeof(Literal));
	literal->token = tok;
	literal->type = token;
	return literal;
};
