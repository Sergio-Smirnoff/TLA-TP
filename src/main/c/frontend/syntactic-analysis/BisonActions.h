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

Program *ProgramSemanticAction(CompilerState *compilerState, Ruleset *ruleset);

// Ruleset
Ruleset *RulesetSemanticAction(Rule *rule, Ruleset *ruleset);

// Rule
Rule *RuleDefinitionSemanticAction(Lexeme_precursor *lexeme, Action *action, Rule_type type);
Rule *RuleNewRegexSemanticAction(char *our_regex_id, Regexes *regex_class, CompilerState *compilerState);

// lexeme precursor
Lexeme_precursor *LexemePrecursorSemanticAction(Lexeme *lex, Lexeme_precursor *lex_prec, Lexeme_chain_type chain_type);
Lexeme_precursor *LexemeDefaultSemanticAction();

// lexeme
Lexeme *LexemeSemanticAction(char *string, Regexes *regex_class, Closure *closure, Lexeme_type type, CompilerState *compilerState);
Lexeme *LexemeStringSemanticAction(char *string);
Lexeme *LexemeClosureSemanticAction(Lexeme_precursor *lex_prec, Closure *closure);

// closure
Closure *ClosureSemanticAction(Token string);

// Regex_class
Regex_class *SymbolRegexSemanticAction(Symbol *symbol);
Regex_class *RegexClassRangeSemanticAction(Symbol *left_symbol, Symbol *right_symbol);
Regex_class *CreatedClassSemanticAction(char *class_name, Closure *closure, CompilerState *compilerState);

// regexes
Regexes *RegexesSemanticAction(Regex_class *regex_class, Regexes *regexes);

Symbol *RegexSymbolSemanticAction(char *string);

// Types
Type *TypeSemanticAction(Token stuff);

// Action
Action *ActionSemanticAction(char *var_name);
Action *ActionJavaSemanticAction(Block *body);

// Java
// NumericComparison
NumericComparison *JavaNumericComparisonSemanticAction(Token token);

// Block
Block *JavaBlockSemanticAction(Statement *state, Block *block);
Block *JavaReturnExpressionSemanticAction(Expression *exp);
Block *JavaThrowExpressionSemanticAction(Expression *exp);

// Statement
Statement *JavaStatementExpressionSemanticAction(StatementExpression *sexp);
Statement *IfStatementSemanticAction(IfThenStatement *ifs);
Statement *WhileStatementSemanticAction(Expression *exp, Block *state);
Statement *ForStatementSemanticAction(ForInit *fors, Expression *exp, StatementExpressionList *list, Block *state);

// ForInit
ForInit *ForInitExpressionListSemanticAction(StatementExpressionList *list);
ForInit *JavaVarTypeDefinitionSemantictAction(Type *type, char *var_name, ForInitType for_type, CompilerState *compilerState);

// StatementExpressionList
StatementExpressionList *StatementExpressionListSemanticAction(StatementExpression *exp, StatementExpressionList *list);

// IfThenStatement
IfThenStatement *JavaIfThenStructureSemanticAction(Expression *exp, Block *ifs, Block *elses);

// StatementExpression
StatementExpression *JavaAsignmentSemanticAction(Assignment *assignment);
StatementExpression *JavaUnaryAccessDefaultSemanticAction(UnaryExpression * unary_expression);
StatementExpression *JavaAsignmentTypeSemanticAction(Type *type, char *var_name, Token java_assignment, Expression *exp);

// VarAccess
VarAccess *VarAccessMethodInvocationSemanticAction(MethodInvocation *method_invocation);
VarAccess *VarAccessVarSemanticAction(char *var_name);
VarAccess *VarAccessVarOperatorSemanticAction(char *var_name, VarAccess *vaccess);
VarAccess *VarAccessTypeOperatorSemanticAction(Type *type, VarAccess *vaccess);

// MethodInvocation
MethodInvocation *InvocationSemanticAction(VarAccess *vaccess, ArgumentList *arglist);

// ArgumentList
ArgumentList *ArgListSemanticExpression(Expression *exp, ArgumentList *arglist);

// Expression
Expression *expressionSematicAction(ConditionalExpression *cexp, Assignment *assignment);

// ConditionalExpression
ConditionalExpression *JavaConditionalExpSemanticAction(ConditionalOrExpression *corexp, Expression *exp, ConditionalExpression *cexp);

// ConditionalOrExpression
ConditionalOrExpression *JavaConditionalOrExpressionSemanticAction(ConditionalAndExpression *candexp, ConditionalOrExpression *corexp);

// ConditionalAndExpression
ConditionalAndExpression *JavaConditionalAndExpressionSemanticAction(ConditionalAndExpression *andexp, EqualityExpression *eqexp);

// EqualityExpression
EqualityExpression *EqualityExpressionSemanticAction(UnaryExpression *uexp, Token token, EqualityExpression *eqexp);

// UnaryExpression
UnaryExpression *UnaryExpressionNumericComparisonSintaticAction(UnaryExpression *uexp1, NumericComparison *numcomp, PostfixExpression *uexp2);
UnaryExpression *UnaryExpressionDoubleTokenSintaticAction(UnaryExpression *uexp1, UnaryExpressionType type, PostfixExpression *uexp2);
UnaryExpression *UnaryExpressionPostfixExpressionSintaticAction(PostfixExpression *pexp);
UnaryExpression *UnaryExpressionTypeSintaticAction(Type *typ);
UnaryExpression *UnaryExpressionSingleTokenSintaticAction(UnaryExpression *uexp, Token token);

// PostfixExpression
PostfixExpression *PostfixExpressionPrimarySemanticAction(Primary *primary);
PostfixExpression *PostfixExpressionVAccessSemanticAction(VarAccess *vaccess, Token token);
PostfixExpression *PostfixExpressionVAccessDefaultSemanticAction(VarAccess *vaccess);

// Assignment
Assignment *AssignmentSemanticAction(VarAccess *vaccess, Token token, Expression *exp);

// Primary
Primary *PrimaryLiteralSemanticAction(Literal *literal);
Primary *PrimaryExpressionSemanticAction(Expression *exp);
Primary *PrimaryCExpSemanticAction(ClassInstanceCreationExpression *cexp);

// ClassInstanceCreationExpression
ClassInstanceCreationExpression *InstanceCreationExpressionSemanticAction(UnqualifiedClassInstanceCreationExpression *exp);
ClassInstanceCreationExpression *VAccessInstanceCreationExpressionSemanticAction(VarAccess *vaccess, UnqualifiedClassInstanceCreationExpression *exp);
ClassInstanceCreationExpression *PrimaryInstanceCreationExpressionSemanticAction(Primary *primary, UnqualifiedClassInstanceCreationExpression *exp);

// UnqualifiedClassInstanceCreationExpression
UnqualifiedClassInstanceCreationExpression *UnqualifiedClassSemanticAction(Type *type, ArgumentList *list);
UnqualifiedClassInstanceCreationExpression *UnqualifiedClassSemanticActionInvocation(MethodInvocation *invocation);

// Literal
Literal *JavaLiteralStrSemanticAction(char *str);
Literal *JavaLiteralTokenSemanticAction(Token token);

#endif
