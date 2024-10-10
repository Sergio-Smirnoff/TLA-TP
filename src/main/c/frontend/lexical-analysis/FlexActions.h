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
// Multiline comment functions:
void BeginMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Regex class names:
void BeginRegexLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginRegexNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
/** 
 * @brief retorna el token OUR_REGEX_ID de clase string con el nombre definido 
 * 
 * @param lexicalAnalyzerContext
 * @return Token
 */
Token ClassNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Regex class content:
void BeginRegexContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token RegexContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token currentContent);

// Lexeme for strings:
void BeginStringLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
/**
 * @brief Retorna el token STR con el contenido de la cadena
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token StringLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Lexeme for regex classes:
void BeginClassLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
/**
 * @brief Retorna el token MIN, MAYUSC, DIGIT, RANGER, SYMOBOL o ESCAPED_SYMBOL
 * de tipo string con el contenido de la clase.
 * 
 * @param lexicalAnalyzerContext 
 * @param currentContent indica el tipo (MIN, MAYUSC, DIGIT, RANGER, SYMOBOL o ESCAPED_SYMBOL)
 * @return Token 
 */
Token RegexContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token currentContent);

// Lexeme for our classes:
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

// Default usage:
void DefaultLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Actions:
void BeginSimpleActionLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginFunctionBodyLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginFunctionParamLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Simple actions:
/**
 * @brief Retorna un token ACTION de tipo string con el nombre del
 * token que se retornaría ante el lexema
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token ActionNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Function param:
/**
 * @brief Retorna el token recibido por param 
 * 
 * @param lexicalAnalyzerContext 
 * @param token (opciones: STRING_TYPE, INTEGER_TYPE, DOUBLE_TYPE, BOOL_TYPE)
 */
Token FunctionParamLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token param);
// Se reutiliza BeginFunctionBodyLexeme

// Function body:
/**
 * @brief Retorna un token BODY de tipo string con el contenido del cuerpo de la función
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token FunctionBodyContentLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void endFunctionBodyLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Endline:
/**
 * @brief Retorna el token ENDLINE.
 * 
 * @param lexicalAnalyzerContext 
 * @return Token 
 */
Token EndlineLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Ignored lexemes:
void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

// Unknown lexeme:
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
