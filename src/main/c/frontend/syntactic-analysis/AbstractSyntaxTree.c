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
		case REGEX:
			free(rule->our_regex_id);
			_releaseRegexes(rule->regexes);
			break;
		case IGNORE_LEXEME:
			_releaseLexemePrecursor(rule->lexeme);
			break;
		case LEXEME_ACTION:
			_releaseLexemePrecursor(rule->lexeme);
			_releaseAction(rule->action);
			break;
		default:
			logError(_logger, "Invalid rule type: %d", rule->type);
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
		case SYMBOL_TYPE:
			_releaseSymbol(regex_class->symbol);
			break;

		case RANGE:
			_releaseSymbol(regex_class->startSymbol);
			_releaseSymbol(regex_class->endSymbol);
			break;

		case VARIABLE:
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
		if (lexeme_precursor->precursor_type != DEFAULT_T)
		{
			switch (lexeme_precursor->chain_type)
			{
			case END:
				_releaseLexeme(lexeme_precursor->lexeme);
				break;

			case CONCATENATION:
			case SUMMATION:
				_releaseLexeme(lexeme_precursor->lexeme);
				_releaseLexemePrecursor(lexeme_precursor->lexeme_precursor);
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
		case REGEXES_TYPE:
			_releaseRegexes(lexeme->regexes);
			_releaseClousure(lexeme->closure);
			break;

		case NAME:
			free(lexeme->our_regex_id);
			_releaseClousure(lexeme->closure);
			break;

		case STRING_LEXEME:
			free(lexeme->string);
			break;

		case PRECURSOR_CLOSURE:
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

void _releaseClousure(Closure *clousure)
{
	if (clousure != NULL)
	{
		free(clousure);
	}
}

// Actions
void _releaseAction(Action *act)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (act != NULL)
	{
		if (act->type == ACTION_T)
		{
			free(act->varName);
		}
		else if (act->type == FUNCTION_BODY)
		{
			_releaseBlock(act->block);
		}
		else
		{
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
		case STATEMENT:
			_releaseStatement(block->statement);
			_releaseBlock(block->block);
			break;

		case RET:
		case THROW:
			_releaseExpression(block->expression);
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
		case STATE_TYPE:
			_releaseStatementExpression(statement->statement_expression);
			break;

		case IF_THEN_STATEMENT:
			_releaseIfThenStatement(statement->if_then_statement);
			break;

		case WHILE_TYPE:
			_releaseExpression(statement->while_expression);
			_releaseBlock(statement->while_block);
			break;

		case FOR_TYPE:
			_releaseForInit(statement->for_init);
			_releaseExpression(statement->for_expression);
			_releaseBlock(statement->for_block);
			_releaseStatementExpressionList(statement->statement_expression_list);
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
		case ASSIGNATION:
			_releaseAssignment(sexp->assignment);
			break;

		case UNARY_ACCESS:
			_releaseUnaryExpression(sexp->unary_expression);
			break;

		case ASSIG_TYPE:
			free(sexp->var_name);
			_releaseExpression(sexp->expression);
			_releaseType(sexp->type);
			break;

		default:
			logError(_logger, "Invalid statement expression type: %d", sexp->state_type);
			break;
		}
		free(sexp);
	}
}

void _releaseIfThenStatement(IfThenStatement *ifThenStatement)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (ifThenStatement != NULL)
	{
		_releaseExpression(ifThenStatement->expression);
		_releaseBlock(ifThenStatement->if_block);
		_releaseBlock(ifThenStatement->else_block);
		free(ifThenStatement);
	}
}

void _releaseForInit(ForInit *forInit)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (forInit != NULL)
	{
		switch (forInit->for_type)
		{
		case STATEMENT_EXPRESSION_LIST:
			_releaseStatementExpressionList(forInit->statement_expression_list);
			break;

		case WITH_TYPES:
			_releaseType(forInit->type);
			free(forInit->var_name_type);
			break;

		case WITHOUT_TYPES:
			free(forInit->var_name);
			break;

		default:
			logError(_logger, "Invalid for init type: %d", forInit->for_type);
			break;
		}
		free(forInit);
	}
}

void _releaseStatementExpressionList(StatementExpressionList *statementExpList)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (statementExpList != NULL)
	{
		_releaseStatementExpression(statementExpList->expression);
		_releaseStatementExpressionList(statementExpList->expression_list);
		free(statementExpList);
	}
}

// Expressions
void _releaseExpression(Expression *exp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (exp != NULL)
	{
		switch (exp->type)
		{
		case CONDITIONAL_EXP:
			_releaseConditionalExpression(exp->conditional_expression);
			break;

		case ASSIGNMENT_TYPE:
			_releaseAssignment(exp->assignment);
			break;

		default:
			logError(_logger, "Invalid expression type: %d", exp->type);
			break;
		}
		free(exp);
	}
}

void _releaseConditionalExpression(ConditionalExpression *condExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (condExp != NULL)
	{
		_releaseConditionalOrExpression(condExp->conditional_or_expression);
		_releaseExpression(condExp->expression);
		_releaseConditionalExpression(condExp->conditional_expression);
		free(condExp);
	}
}

void _releaseConditionalOrExpression(ConditionalOrExpression *condOrExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (condOrExp != NULL)
	{
		_releaseConditionalAndExpression(condOrExp->conditional_and_expression);
		_releaseConditionalOrExpression(condOrExp->conditional_or_expression);
		free(condOrExp);
	}
}

void _releaseConditionalAndExpression(ConditionalAndExpression *condAndExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (condAndExp != NULL)
	{
		_releaseEqualityExpression(condAndExp->equality_expression);
		_releaseConditionalAndExpression(condAndExp->conditional_and_expression);
		free(condAndExp);
	}
}

void _releaseEqualityExpression(EqualityExpression *eqExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (eqExp != NULL)
	{
		_releaseUnaryExpression(eqExp->unary_expression);
		_releaseEqualityExpression(eqExp->equality_expression);
		free(eqExp);
	}
}

void _releaseUnaryExpression(UnaryExpression *unExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (unExp != NULL)
	{
		switch (unExp->globaltype)
		{
		case NUMERIC_COMPARISON:
			_releaseUnaryExpression(unExp->num_comp_unary_exp1);
			_releaseNumericComparison(unExp->numcomp);
			_releasePostfixExpression(unExp->num_comp_unary_exp2);
			break;

		case DOUBLE_TOKEN:
			_releaseUnaryExpression(unExp->uexp_unary_expression1);
			_releasePostfixExpression(unExp->uexp_unary_expression2);
			break;

		case POSTFIX_EXPRESSION:
			_releasePostfixExpression(unExp->postfix_expression);
			break;

		case TYPE:
			_releaseType(unExp->object_type);
			break;

		case SINGLE_TOKEN:
			_releaseUnaryExpression(unExp->unary_expression);
			break;
		default:
			logError(_logger, "Invalid unary expression type: %d", unExp->globaltype);
			break;
		}
		free(unExp);
	}
}

void _releasePostfixExpression(PostfixExpression *postExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (postExp != NULL)
	{
		_releasePrimary(postExp->primary);
		_releaseVarAccess(postExp->vaccess);
		free(postExp);
	}
}

void _releaseUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression *unqualClassInstCreationExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (unqualClassInstCreationExp != NULL)
	{
		switch (unqualClassInstCreationExp->unq_type)
		{
		case PARARGS_TYPE:
			_releaseType(unqualClassInstCreationExp->type);
			_releaseArgumentList(unqualClassInstCreationExp->arglist);
			break;

		case METHOD_TYPE:
			_releaseMethodInvocation(unqualClassInstCreationExp->invocation);
			break;

		default:
			logError(_logger, "Invalid unqualified class instance creation expression type: %d", unqualClassInstCreationExp->unq_type);
			break;
		}
		free(unqualClassInstCreationExp);
	}
}

void _releaseClassInstanceCreationExpression(ClassInstanceCreationExpression *classInstCreationExp)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (classInstCreationExp != NULL)
	{
		_releaseUnqualifiedClassInstanceCreationExpression(classInstCreationExp->unq_class_inst_creation_exp);
		_releaseVarAccess(classInstCreationExp->vaccess);
		_releasePrimary(classInstCreationExp->primary);
		free(classInstCreationExp);
	}
}

void _releasePrimary(Primary *primary)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (primary != NULL)
	{
		switch (primary->type)
		{
		case LITERAL_TYPE:
			_releaseLiteral(primary->literal);
			break;

		case EXPRESSION_TYPE:
			_releaseExpression(primary->expression);
			break;

		case CONDITIONAL_EXPRESSION_TYPE:
			_releaseClassInstanceCreationExpression(primary->class_inst_creation_exp);
			break;

		default:
			logError(_logger, "Invalid primary type: %d", primary->type);
			break;
		}
		free(primary);
	}
}

void _releaseLiteral(Literal *literal)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (literal != NULL)
	{
		switch (literal->type)
		{
		case STRING_T:
			free(literal->str);
			break;

		case TOKEN_T:
			break;

		default:
			logError(_logger, "Invalid literal type: %d", literal->type);
			break;
		}
		free(literal);
	}
}

// Assignment
void _releaseAssignment(Assignment *assignment)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (assignment != NULL)
	{
		_releaseVarAccess(assignment->vaccess);
		_releaseExpression(assignment->expression);
		free(assignment);
	}
}

// Other stuff
void _releaseNumericComparison(NumericComparison *numericComparison)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (numericComparison != NULL)
	{
		free(numericComparison);
	}
}

void _releaseVarAccess(VarAccess *var_access)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (var_access != NULL)
	{
		free(var_access->var_name);
		_releaseVarAccess(var_access->vaccess);
		_releaseType(var_access->type);
		_releaseMethodInvocation(var_access->method_invocation);
		free(var_access);
	}
}

void _releaseMethodInvocation(MethodInvocation *methodInvocation)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (methodInvocation != NULL)
	{
		_releaseVarAccess(methodInvocation->vaccess);
		_releaseArgumentList(methodInvocation->arglist);
		free(methodInvocation);
	}
}

void _releaseArgumentList(ArgumentList *argList)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (argList != NULL)
	{
		_releaseExpression(argList->expression);
		_releaseArgumentList(argList->arglist);
		free(argList);
	}
}

void _releaseFunctionBody(Function_body *functionBody)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (functionBody != NULL)
	{
		free(functionBody->log);
		free(functionBody->ret);
		free(functionBody);
	}
}

void _releaseType(Type *type)
{
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (type != NULL)
	{
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
