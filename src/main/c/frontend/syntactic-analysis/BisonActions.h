#ifndef BISON_ACTIONS_HEADER
#define BISON_ACTIONS_HEADER

#include "../../shared/CompilerState.h"
#include "../../shared/Logger.h"
#include "../../shared/Type.h"
#include "AbstractSyntaxTree.h"
#include "SyntacticAnalyzer.h"
#include <stdlib.h>

/** Initialize module's internal state. */
void initializeBisonActionsModule();

/** Shutdown module's internal state. */
void shutdownBisonActionsModule();

/**
 * Bison semantic actions.
 */

// new

//Regex * RegexSemanticAction(char* action_name, char * action, RegexType type);
Program * ProgramSemanticAction(CompilerState * compilerState, Regex * expression);
// lexeme
Lexeme LexemeSemanticAction( char* string, Regex_class* regex_class, Closure* closure, Lexeme_type type );

// closure
Closure ClosureSemanticAction( char* string );

// range
Range RangeSemanticAction( char* right, char* left );

// Params
Param ParamSemanticAction( Token* token );

// Action
Action ActionSemanticAction( char* string );
Action ActionParamSemanticAction( Param* param, char* body );

//Regex_class

Regex_class RegexClassStringSemanticAction( char* string, Regex_class* regex_class );
Regex_class RegexClassRangeSemanticAction( Range* range, Regex_class* regex_class );


// old
/*
Constant * IntegerConstantSemanticAction(const int value);
Expression * ArithmeticExpressionSemanticAction(Expression * leftExpression, Expression * rightExpression, ExpressionType type);
Expression * FactorExpressionSemanticAction(Factor * factor);
Factor * ConstantFactorSemanticAction(Constant * constant);
Factor * ExpressionFactorSemanticAction(Expression * expression);
Program * ExpressionProgramSemanticAction(CompilerState * compilerState, Expression * expression);
*/
#endif
