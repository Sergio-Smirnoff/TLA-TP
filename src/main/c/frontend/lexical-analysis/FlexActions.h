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

void BeginActionLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndActionLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginActionDooerLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndActionDooerLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token NameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token name);
Token ActionNameLexeme(LexicalAnalyzerContext * lexicalAnalyzerContext, Token action_id);

void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token UnknownLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);


// OLD, PORLAS
/*
void BeginMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token ArithmeticOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);
Token IntegerLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ParenthesisLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);
*/


#endif
