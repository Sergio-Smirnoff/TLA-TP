#ifndef FLEX_ACTIONS_HEADER
#define FLEX_ACTIONS_HEADER

#include "../../shared/Environment.h"
#include "../../shared/Logger.h"
#include "../../shared/String.h"
#include "../../shared/Type.h"
#include "../syntactic-analysis/AbstractSyntaxTree.h"
#include "../syntactic-analysis/BisonParser.h"
#include "LexicalAnalyzerContext.h"
#include <stdio.h>
#include <stdlib.h>

/** Initialize module's internal state. */
void initializeFlexActionsModule();

/** Shutdown module's internal state. */
void shutdownFlexActionsModule();

/**
 * Flex lexeme processing actions.
 */

// NEW, actions are interpreted in Flex2Actions.h
// Comments:
// Multiline comment functions:
void BeginMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Line comment functions:
void BeginLineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndLineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Regex class names functions:
void BeginRegexLine(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginRegexNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
/** 
 * @brief retorna el token OUR_REGEX_ID de clase string con el nombre definido 
 * 
 * @param lexicalAnalyzerContext
 * @return Token
 */
Token RegexClassNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Regex class content functions:
void BeginRegexContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
/**
 * @brief Retorna el token LOWER, UPPER, DIGIT, RANGER, SYMOBOL o ESCAPED_SYMBOL
 * de tipo string con el contenido de la clase.
 * 
 * @param lexicalAnalyzerContext 
 * @param currentContent indica el tipo (LOWER, UPPER, DIGIT, RANGER, SYMOBOL o ESCAPED_SYMBOL)
 * @return Token 
 */
Token RegexContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

// Lexeme for strings functions:
void BeginStringLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
/**
 * @brief Retorna el token STR con el contenido de la cadena
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token StringLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndStringLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Lexeme for regex classes functions:
void BeginClassLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
// se reutiliza RegexContentLexeme

// Lexeme for our classes functions:
void BeginOurClassLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
// se reutiliza ClassNameLexeme
/**
 * @brief Retorna el token de tipo token que indica que clausura se usó
 * 
 * @param lexicalAnalyzerContext 
 * @param token el tipo de clausura (KLEENE, POSITIVE)
 * @return Token 
 */
Token ClauseOperator(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

// Default usage functions:
void DefaultLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Actions functions:
void BeginSimpleActionLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginFunctionBodyLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginFunctionParamLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Simple actions functions:
/**
 * @brief Retorna un token ACTION de tipo string con el nombre del
 * token que se retornaría ante el lexema
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token ActionNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Function param functions:
/**
 * @brief Retorna el token recibido por param 
 * 
 * @param lexicalAnalyzerContext 
 * @param token (opciones: STRING_TYPE, INTEGER_TYPE, DOUBLE_TYPE, BOOL_TYPE)
 */
Token FunctionParamLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token param);
// Se reutiliza BeginFunctionBodyLexeme

// Function body functions:
/**
 * @brief Retorna un token BODY de tipo string con el contenido del cuerpo de la función
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token FunctionBodyContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndFunctionBodyLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Endline functions:
/**
 * @brief Retorna el token ENDLINE.
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token EndLineLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Unknown lexeme functions:
Token UnknownLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

// OLD, PORLAS
/*
Token ArithmeticOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);
Token IntegerLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ParenthesisLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

void BeginActionLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndActionLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginActionDooerLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndActionDooerLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token NameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token name);
Token ActionNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token action_id);
*/


#endif
