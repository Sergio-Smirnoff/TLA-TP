#include "FlexActions.h"

/* MODULE INTERNAL STATE */

static Logger *_logger = NULL;
static boolean _logIgnoredLexemes = true;

void initializeFlexActionsModule()
{
	_logIgnoredLexemes = getBooleanOrDefault("LOG_IGNORED_LEXEMES", _logIgnoredLexemes);
	_logger = createLogger("FlexActions");
}

void shutdownFlexActionsModule()
{
	if (_logger != NULL)
	{
		destroyLogger(_logger);
	}
}

/* PRIVATE FUNCTIONS */

static void _logLexicalAnalyzerContext(const char *functionName, LexicalAnalyzerContext *lexicalAnalyzerContext);

/**
 * Logs a lexical-analyzer context in DEBUGGING level.
 */
static void _logLexicalAnalyzerContext(const char *functionName, LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	char *escapedLexeme = escape(lexicalAnalyzerContext->lexeme);
	logDebugging(_logger, "%s: %s (context = %d, length = %d, line = %d)",
				 functionName,
				 escapedLexeme,
				 lexicalAnalyzerContext->currentContext,
				 lexicalAnalyzerContext->length,
				 lexicalAnalyzerContext->line);
	free(escapedLexeme);
}

/* PUBLIC FUNCTIONS */

// new
// Comments:
// Multiline comment functions:
void BeginMultilineCommentLexemeAction(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

void EndMultilineCommentLexemeAction(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

void IgnoredLexemeAction(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

// Line comment functions:
void BeginLineCommentLexemeAction(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

void EndLineCommentLexemeAction(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

// Basic utility:
Token SimpleTokenInsert(LexicalAnalyzerContext *lexicalAnalyzerContext, Token token)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	// lexicalAnalyzerContext->semanticValue->token = token;
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token SimpleStringInsert(LexicalAnalyzerContext *lexicalAnalyzerContext, Token token)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

// Regex class names functions:
void BeginRegexLine(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

void BeginRegexNameLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

Token VarNameLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return VAR_NAME;
}

// Regex class content functions:
void BeginRegexContentLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

Token RegexContentLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext, Token token)
{
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

// Lexeme for strings functions:
void BeginStringLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

Token StringLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return STR;
}

void EndStringLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

// Lexeme for regex classes functions:
void BeginClassLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

// Lexeme for our classes functions:
void BeginOurClassLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

Token ClauseOperator(LexicalAnalyzerContext *lexicalAnalyzerContext, Token clause)
{
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = clause;
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return clause;
}

// Default usage functions:
Token DefaultLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return DEFAULT;
}

// Actions functions:
void BeginSimpleActionLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

void BeginFunctionBodyLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

void BeginFunctionTypeLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

// Function type functions:
Token FunctionTypeLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext, Token type)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, type);
}

void EndFunctionBodyLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	if (_logIgnoredLexemes)
	{
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	}
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
}

// Endline functions:
Token EndLineLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, ENDLINE);
}

Token UnknownLexemeAction(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	return UNKNOWN;
}

Token OpenParenthesesLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, OPEN_PARENTHESES);
}

Token CloseParenthesesLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, CLOSE_PARENTHESES);
}

Token OpenBracesLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, OPEN_BRACES);
}

Token CloseBracesLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, CLOSE_BRACES);
}

Token ArrowLexeme(LexicalAnalyzerContext *lexicalAnalyzerContext)
{
	return SimpleTokenInsert(lexicalAnalyzerContext, ARROW);
}

// old
/*

Token ArithmeticOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token IntegerLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->integer = atoi(lexicalAnalyzerContext->lexeme);
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return INTEGER;
}

Token ParenthesisLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}
*/
