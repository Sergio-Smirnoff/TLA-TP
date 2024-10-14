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

Program * ProgramSemanticAction(CompilerState * compilerState, Ruleset * ruleset);

//Ruleset
Ruleset* RulesetSemanticAction( Rule* rule, Ruleset* ruleset );

//Rule
Rule* RuleDefinitionSemanticAction( Lexeme_precursor* lexeme, Action* action, Token endline, Rule_type type );
Rule* RuleNewRegexSemanticAction( char* our_regex_id, Regex_class* regex_class, Token endline );

// lexeme
Lexeme* LexemeSemanticAction( char* string, Regex_class* regex_class, Closure* closure, Lexeme_type type );

// closure
Closure* ClosureSemanticAction( Token string );

// range
Range* RangeSemanticAction( char* right, char* left );

// Params
Param* ParamSemanticAction( Token stuff );

// Action
Action* ActionSemanticAction( char* string );
Action* ActionParamSemanticAction( Param* param, char* body );

//Regex_class
Regex_class* RegexClassStringSemanticAction( char* string, Regex_class* regex_class );
Regex_class* RegexClassRangeSemanticAction( Range* range, Regex_class* regex_class );

Lexeme_precursor* LexemePrecursorSemanticAction( Lexeme *lex, Lexeme_precursor *lex_prec );


NumericComparison* JavaNumericComparisonSemanticAction(Token token);


Block* JavaBlockSemanticAction( Statement* statment, Block* block );


Statement* InlineStatementSemanticAction(StatementWithoutTrailingSubstatement* inline);
Statement* IfStatementSemanticAction ( IfThenStatement* if );
Statement* IfElseStatement ( ifThenElseStatement* ifelse );
Statement* WhileStatementSemanticAction(Expression* exp, Statement* state);
Statement* ForStatementSemanticAction(ForInit* for, Expression* exp, StatementExpresionList* list, Statement* state);

ForInit* StatementExpressionListSemanticAction(StatementExpresionList* list)
ForInit* JavaVarTypeDefinitionSemantictAction(Param* param, char* var_name)


StatementExpressionList* StatementExpressionListSemanticAction( StatementExpression* exp, StatementExpressionList* list );

IfThenStatement* JavaIfThenStructureSemanticAction( Expression* exp, Statement* if, Statement* else );
IfThenElseStatement* JavaIfThenElseStructureSemanticAction( Expression* exp, Statement* if, Statement* else );


StatementWithoutTrailingSubstatement* JavaStatementExpressionSemanticAction ( StatementExpression* sexp );
StatementWithoutTrailingSubstatement* JavaReturnExpressionSemanticAction( Expression* exp );
StatementWithoutTrailingSubstatement* JavaThrowExpressionSemanticAction( Expression* exp );


StatementExpression* JavaAsignmentSemanticAction(Assignment* assignment);
StatementExpression* JavaModifyingStatementExpressionSemanticAction( StateExpressionType type, Token token, UnaryExpression* exp );
StatementExpression* JavaMethodInvocationSemanticAction(MethodInvocation* method_invocation);
StatementExpression* JavaAsignmentParamSemanticAction( Param* param, char* var_name, Token java_assignment, Expression* exp );


VarAccess* VarAccessMethodInvocationSemanticAction( MethodInvocation* method_invocation );
VarAccess* VarAccessVarSemanticAction( char* var_name );
VarAccess* VarAccessVarOperatorSemanticAction( char* var_name, VarAccess* vaccess );
VarAccess* VarAccessParamOperatorSemanticAction( Param* param, VarAccess* vaccess );


MethodInvocation* InvocationSemanticAction( VarAccess* vaccess, ArgumentList* arglist );

ArgumentList* ArgListSemanticExpression( Expression* exp; ArgumentList* arglist );


Expression* expressionSematicAction( ConditionalExpression* cexp, Assignment* assignment );

ConditionalExpression* JavaConditionalExpSemanticAction( ConditionalOrExpression* corexp, Expression* exp, ConditionalExpression* cexp );


ConditionalOrExpression* JavaConditionalOrExpressionSemanticAction( ConditionalAndExpression* candexp, ConditionalOrExpression* corexp );


ConditionalAndExpression* JavaConditionalAndExpressionSemanticAction( ConditionalAndExpression* andexp, EqualityExpression* eqexp );

EqualityExpression* EqualityExpressionSemanticAction( UnaryExpression * uexp, EqualityExpression* eqexp );

UnaryExpression* UnaryExpressionNumericComparisonSintaticAction( UnaryExpression* uexp1, NumericComparison* numcomp, UnaryExpression* uexp2 );
UnaryExpression* UnaryExpressionDoubleTokenSintaticAction( UnaryExpression* uexp1, UnaryExpressionType type, UnaryExpression* uexp2 );
UnaryExpression* UnaryExpressionPostfixExpressionSintaticAction( PostfixExpression* pexp );
UnaryExpression* UnaryExpressionParamSintaticAction( Param* param);
UnaryExpression* UnaryExpressionSingleTokenSintaticAction( UnaryExpression* uexp, Token token);

PostfixExpression* PostfixExpressionPrimarySemanticAction( Primary* primary );
PostfixExpression* PostfixExpressionVAccessSemanticAction( VarAccess* vaccess );
PostfixExpression* PostfixExpressionSemanticAction( VarAccess* vaccess, Token token );


Assignment* AssignmentSemanticAction( VarAccess* vaccess, Expression* exp );


Primary* PrimaryLiteralSemanticAction( Literal* literal );
Primary* PrimaryExpressionSemanticAction( Expression* exp );
Primary* PrimaryCExpSemanticAction( ClassInstanceCreationExpression* cexp );


ClassInstanceCreationExpression* InstanceCreationExpressionSemanticAction( UnqualifiedClassInstanceCreationExpression* exp );
ClassInstanceCreationExpression* VAccessInstanceCreationExpressionSemanticAction( VarAccess* vaccess, UnqualifiedClassInstanceCreationExpression* exp );
ClassInstanceCreationExpression* PrimaryInstanceCreationExpressionSemanticAction( Primary* primary, UnqualifiedClassInstanceCreationExpression* exp );

UnqualifiedClassInstanceCreationExpression* UnqualifiedClassSemanticAction( Param* param, ArgumentList* list )

Literal* JavaLiteralStrSemanticAction( char* str );
Literal* JavaLiteralTokenSemanticAction( Token token  );

#endif
