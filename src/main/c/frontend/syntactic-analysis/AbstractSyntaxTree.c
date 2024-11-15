#include "AbstractSyntaxTree.h"

/* MODULE INTERNAL STATE */

static Logger *_logger = NULL;

void initializeAbstractSyntaxTreeModule()
{
	_logger = createLogger("AbstractSyntxTree");
}

void shutdownAbstractSyntaxTreeModule()
{
	if (_logger != NULL)
	{
		destroyLogger(_logger);
	}
}

/** PRIVATE FUNCTIONS */
// Rules
void _releaseRuleset(Ruleset *ruleset);
void _releaseRule(Rule *rule);

// Regexes
void _releaseRegexes(Regexes *regexes);
void _releaseRegexClass(Regex_class *regex_class);

// Lexemes
void _releaseLexemePrecursor(Lexeme_precursor *lexeme_precursor);
void _releaseLexeme(Lexeme *lexeme);

// Multipurpose (Regexes and Lexemes)
void _releaseSymbol(Symbol *symbol);
void _releaseClousure(Closure *closure);

// Actions
void _releaseAction(Action *act);

// Java
void _releaseBlock(Block *block);

// Statements
void _releaseStatement(Statement *state);
void _releaseStatementExpression(StatementExpression *sexp);
void _releaseIfThenStatement(IfThenStatement *ifThenStatement);
void _releaseForInit(ForInit *forInit);
void _releaseStatementExpressionList(StatementExpressionList *statementExpList);

// Expressions
void _releaseExpression(Expression *exp);
void _releaseConditionalExpression(ConditionalExpression *condExp);
void _releaseConditionalOrExpression(ConditionalOrExpression *condOrExp);
void _releaseConditionalAndExpression(ConditionalAndExpression *condAndExp);
void _releaseEqualityExpression(EqualityExpression *eqExp);
void _releaseUnaryExpression(UnaryExpression *unExp);
void _releasePostfixExpression(PostfixExpression *postExp);
void _releaseUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression *unqualClassInstCreationExp);
void _releaseClassInstanceCreationExpression(ClassInstanceCreationExpression *classInstCreationExp);
void _releasePrimary(Primary *primary);
void _releaseLiteral(Literal *literal);

// Assignment
void _releaseAssignment(Assignment *assignment);

// Other stuff
void _releaseNumericComparison(NumericComparison *numericComparison);
void _releaseVarAccess(VarAccess *var_access);
void _releaseMethodInvocation(MethodInvocation *methodInvocation);
void _releaseArgumentList(ArgumentList *argList);
void _releaseFunctionBody(Function_body *functionBody);
void _releaseType(Type *type);

// Rules
void _releaseRuleset(Ruleset *ruleset)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (ruleset != NULL)
	{
		_releaseRule(ruleset->rule);
		_releaseRuleset(ruleset->ruleset);
		free(ruleset);
	}
}

void _releaseRule(Rule *rule)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (rule != NULL)
	{
		switch (rule->type)
		{
		case regex:
			free(rule->our_regex_id);
			_releaseRegexes(rule->regexes);
			break;
		case ignore_lexeme:
			_releaseLexemePrecursor(rule->lexeme);
			break;
		case lexeme_action:
			_releaseLexemePrecursor(rule->lexeme);
			_releaseAction(rule->action);
			break;
		}
		free(rule);
	}
}

// Regexes
void _releaseRegexes(Regexes *regexes)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (regexes != NULL)
	{
		_releaseRegexClass(regexes->regexClass);
		_releaseRegexes(regexes->regexes);
		free(regexes);
	}
}

void _releaseRegexClass(Regex_class *regex_class)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (regex_class != NULL)
	{
		switch (regex_class->type)
		{
		case symbol:
			_releaseSymbol(regex_class->symbol);
			break;

		case range:
			_releaseSymbol(regex_class->startSymbol);
			_releaseSymbol(regex_class->endSymbol);
			break;

		case variable:
			free(regex_class->varName);
			_releaseClousure(regex_class->closure);
			break;

		default:
			logError(_logger, "Invalid regex class type: %d", regex_class->type);
			break;
		}
		free(regex_class);
	}
}

// Lexemes
void _releaseLexemePrecursor(Lexeme_precursor *lexeme_precursor)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (lexeme_precursor != NULL)
	{
		if(lexeme_precursor->precursor_type != default_t)
		{
			switch(lexeme_precursor->chain_type)
			{
			case end:
				_releaseLexeme(lexeme_precursor->lex);
				break;

			case concatenation:
			case summation:
				_releaseLexeme(lexeme_precursor->lex);
				_releaseLexemePrecursor(lexeme_precursor->lex_prec);
				break;

			default:
				logError(_logger, "Invalid lexeme precursor chain type: %d", lexeme_precursor->chain_type);
				break;
			}
		}
		free(lexeme_precursor);
	}
}

void _releaseLexeme(Lexeme *lexeme)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (lexeme != NULL)
	{
		switch (lexeme->type)
		{
		case regexes:
			_releaseRegexes(lexeme->regexes);
			_releaseClousure(lexeme->closure);
			break;

		case name:
			free(lexeme->our_regex_id);
			_releaseClousure(lexeme->closure);
			break;

		case string_lexeme:
			free(lexeme->string);
			break;

		case precursor_closure:
			_releaseLexemePrecursor(lexeme->precursor);
			_releaseClousure(lexeme->closure);
			break;
		
		default:
			logError(_logger, "Invalid lexeme type: %d", lexeme->type);
			break;
		}
		free(lexeme);
	}
}

// Multipurpose (Regexes and Lexemes)
void _releaseSymbol(Symbol *symbol)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (symbol != NULL)
	{
		free(symbol->symbol_tok);
		free(symbol);
	}
}

void _releaseClousure(Closure *clousure) {
	if(clousure != NULL) {
		free(clousure);
	}
}

// Actions
void _releaseAction(Action *act)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (act != NULL)
	{
		if (act->type == action)
		{
			free(act->varName);
		} else if (act->type == function_body)
		{
			_releaseBlock(act->block);
		} else {
			logError(_logger, "Invalid action type: %d", act->type);
		}
		free(act);
	}
}

// Java
void _releaseBlock(Block *block)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (block != NULL)
	{
		switch (block->type)
		{
		case statement:
			_releaseStatement(block->statement);
			_releaseBlock(block->block);
			break;

		case ret:
		case throw:
			_releaseExpression(block->exp);
			break;

		default:
			logError(_logger, "Invalid block type: %d", block->type);
			break;
		}
		free(block);
	}
}

// Statements
void _releaseStatement(Statement *statement)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (statement != NULL)
	{
		switch (statement->type)
		{
		case state:
			_releaseStatementExpression(statement->sexp);
			break;

		case ifThenStatement:
			_releaseIfThenStatement(statement->ifThen);
			break;

		case While:
			_releaseExpression(statement->expwhile);
			_releaseBlock(statement->blockwhile);
			break;

		case For:
			_releaseForInit(statement->forInit);
			_releaseExpression(statement->expfor);
			_releaseBlock(statement->blockfor);
			_releaseStatementExpressionList(statement->statementExpList);
			break;

		default:
			logError(_logger, "Invalid statement type: %d", statement->type);
			break;
		}
		free(statement);
	}
}

void _releaseStatementExpression(StatementExpression *sexp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (sexp != NULL)
	{
		switch (sexp->state_type)
		{
		case assignation:
			_releaseAssignment(sexp->assignment);
			break;
		
		case vaccess:
			_releaseVarAccess(sexp->var_access);
			break;

		case assigType:
			free(sexp->var_name);
			_releaseExpression(sexp->exp);
			_releaseType(sexp->type);
			break;

		default:
			logError(_logger, "Invalid statement expression type: %d", sexp->state_type);
			break;
		}
		free(sexp);
	}
}

void _releaseIfThenStatement(IfThenStatement *ifThenStatement) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(ifThenStatement != NULL) {
		_releaseExpression(ifThenStatement->exp);
		_releaseBlock(ifThenStatement->ifblock);
		_releaseBlock(ifThenStatement->elseblock);
		free(ifThenStatement);
	}
}

void _releaseForInit(ForInit *forInit) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(forInit != NULL) {
		switch(forInit->for_type) {
		case statementExpList:
			_releaseStatementExpressionList(forInit->statementExpList);
			break;

		case withTypes:
			_releaseType(forInit->type);
			free(forInit->var_name_type);
			break;

		case withoutTypes:
			free(forInit->var_name);
			break;

		default:
			logError(_logger, "Invalid for init type: %d", forInit->for_type);
			break;
		}
		free(forInit);
	}
}

void _releaseStatementExpressionList(StatementExpressionList *statementExpList) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(statementExpList != NULL) {
		_releaseStatementExpression(statementExpList->exp);
		_releaseStatementExpressionList(statementExpList->list);
		free(statementExpList);
	}
}

// Expressions
void _releaseExpression(Expression *exp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(exp != NULL) {
		switch(exp->type) {
		case xexp:
			_releaseConditionalExpression(exp->xexp);
			break;

		case assignment:
			_releaseAssignment(exp->assignment);
			break;

		default:
			logError(_logger, "Invalid expression type: %d", exp->type);
			break;
		}
		free(exp);
	}
}

void _releaseConditionalExpression(ConditionalExpression *condExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(condExp != NULL) {
		_releaseConditionalOrExpression(condExp->corexp);
		_releaseExpression(condExp->exp);
		_releaseConditionalExpression(condExp->cexp);
		free(condExp);
	}
}

void _releaseConditionalOrExpression(ConditionalOrExpression *condOrExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(condOrExp != NULL) {
		_releaseConditionalAndExpression(condOrExp->candexp);
		_releaseConditionalOrExpression(condOrExp->corexp);
		free(condOrExp);
	}
}

void _releaseConditionalAndExpression(ConditionalAndExpression *condAndExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(condAndExp != NULL) {
		_releaseEqualityExpression(condAndExp->eqexp);
		_releaseConditionalAndExpression(condAndExp->candexp);
		free(condAndExp);
	}
}

void _releaseEqualityExpression(EqualityExpression *eqExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(eqExp != NULL) {
		_releaseUnaryExpression(eqExp->uexp);
		_releaseEqualityExpression(eqExp->eqexp);
		free(eqExp);
	}
}

void _releaseUnaryExpression(UnaryExpression *unExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(unExp != NULL) {
		switch(unExp->globaltype) {
		case numericComparison:
			_releaseUnaryExpression(unExp->uexp1_num);
			_releaseNumericComparison(unExp->numcomp);
			_releasePostfixExpression(unExp->uexp2_num);
			break;

		case doubleToken:
			_releaseUnaryExpression(unExp->uexp1_exp);
			_releasePostfixExpression(unExp->uexp2_exp);
			break;

		case postfixExpression:
			_releasePostfixExpression(unExp->pexp);
			break;

		case type:
			_releaseType(unExp->obj_type);
			break;

		case singleToken:
			_releaseUnaryExpression(unExp->uexp);

		default:
			logError(_logger, "Invalid unary expression type: %d", unExp->globaltype);
			break;
		}
		free(unExp);
	}
}

void _releasePostfixExpression(PostfixExpression *postExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(postExp != NULL) {
		_releasePrimary(postExp->primary);
		_releaseVarAccess(postExp->vaccess);
		free(postExp);
	}
}

void _releaseUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression *unqualClassInstCreationExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(unqualClassInstCreationExp != NULL) {
		switch(unqualClassInstCreationExp->unq_type) {
		case parargs:
			_releaseType(unqualClassInstCreationExp->type);
			_releaseArgumentList(unqualClassInstCreationExp->arglist);
			break;

		case method:
			_releaseMethodInvocation(unqualClassInstCreationExp->invocation);
			break;

		default:
			logError(_logger, "Invalid unqualified class instance creation expression type: %d", unqualClassInstCreationExp->unq_type);
			break;
		}
		free(unqualClassInstCreationExp);
	}
}

void _releaseClassInstanceCreationExpression(ClassInstanceCreationExpression *classInstCreationExp) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(classInstCreationExp != NULL) {
		_releaseUnqualifiedClassInstanceCreationExpression(classInstCreationExp->ucice);
		_releaseVarAccess(classInstCreationExp->vaccess);
		_releasePrimary(classInstCreationExp->primary);
		free(classInstCreationExp);
	}
}

void _releasePrimary(Primary *primary) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(primary != NULL) {
		switch(primary->type) {
		case literal:
			_releaseLiteral(primary->lit);
			break;

		case expression:
			_releaseExpression(primary->exp);
			break;

		case cexp:
			_releaseClassInstanceCreationExpression(primary->cice);
			break;

		default:
			logError(_logger, "Invalid primary type: %d", primary->type);
			break;
		}
		free(primary);
	}
}

void _releaseLiteral(Literal *literal) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(literal != NULL) {
		switch(literal->type) {
		case str:
			free(literal->str);
			break;

		case token:
			break;

		default:
			logError(_logger, "Invalid literal type: %d", literal->type);
			break;
		}
		free(literal);
	}
}

// Assignment
void _releaseAssignment(Assignment *assignment) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(assignment != NULL) {
		_releaseVarAccess(assignment->vaccess);
		_releaseExpression(assignment->expression);
		free(assignment);
	}
}

// Other stuff
void _releaseNumericComparison(NumericComparison *numericComparison) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(numericComparison != NULL) {
		free(numericComparison);
	}
}

void _releaseVarAccess(VarAccess *var_access) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(var_access != NULL) {
		free(var_access->var_name);
		_releaseVarAccess(var_access->vaccess);
		_releaseType(var_access->type);
		_releaseMethodInvocation(var_access->method_invocation);
		free(var_access);
	}
}

void _releaseMethodInvocation(MethodInvocation *methodInvocation) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(methodInvocation != NULL) {
		_releaseVarAccess(methodInvocation->vaccess);
		_releaseArgumentList(methodInvocation->arglist);
		free(methodInvocation);
	}
}

void _releaseArgumentList(ArgumentList *argList) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(argList != NULL) {
		_releaseExpression(argList->expression);
		_releaseArgumentList(argList->arglist);
		free(argList);
	}
}

void _releaseFunctionBody(Function_body *functionBody) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(functionBody != NULL) {
		free(functionBody->log);
		free(functionBody->ret);
		free(functionBody);
	}
}

void _releaseType(Type *type) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if(type != NULL) {
		free(type);
	}
}

/** PUBLIC FUNCTIONS */
void releaseProgram(Program *program)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (program != NULL)
	{
		_releaseRuleset(program->ruleset);
		free(program);
	}
}
