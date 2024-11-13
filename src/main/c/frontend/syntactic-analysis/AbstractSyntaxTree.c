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

// Actions
void _releaseAction(Action *act);

// Java
void _releaseBlock(Block *block);

// Statements
void _releaseStatement(Statement *state);
void _releaseStatementExpression(StatementExpression *sexp);

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
			// Closure release not needed, it is an int
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
			// Closure release not needed, it is an int
			break;

		case name:
			free(lexeme->our_regex_id);
			// Closure release not needed, it is an int
			break;

		case string_lexeme:
			free(lexeme->string);
			break;

		case precursor_closure:
			_releaseLexemePrecursor(lexeme->precursor);
			// Closure release not needed, it is an int
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
			// _releaseExpression(block->exp);
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
	if (state != NULL)
	{
		switch (statement->type)
		{
		case state:
			_releaseStatementExpression(statement->sexp);
			break;

		case ifThenStatement:
			// _releaseIfThenStatement(statement->ifThen);
			break;

		case While:
			// _releaseExpression(statement->expwhile);
			_releaseBlock(statement->blockwhile);
			break;

		case For:
			// _releaseForInit(statement->forInit);
			// _releaseExpression(statement->expfor);
			_releaseBlock(statement->blockfor);
			// _releaseStatementExpressionList(statement->statementExpList);
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
			// _releaseAssignment(sexp->assignment);
			break;
		
		case vaccess:
			// _releaseVarAccess(sexp->var_access);
			break;

		case assigType:
			// _releaseType(sexp->type);
			free(sexp->var_name);
			// _releaseExpression(sexp->exp);
			break;

		default:
			logError(_logger, "Invalid statement expression type: %d", sexp->state_type);
			break;
		}
		free(sexp);
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
