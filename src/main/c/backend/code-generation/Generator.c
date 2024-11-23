#include "Generator.h"

#define MAX_UINT64_LENGTH 32

/* MODULE INTERNAL STATE */

const char _indentationCharacter = ' ';
const char _indentationSize = 4;
static Logger *_logger = NULL;
static FILE *_outputFile = NULL;

void initializeGeneratorModule()
{
    _logger = createLogger("Generator");
    _outputFile = fopen("Automaton.java", "w");
}

void shutdownGeneratorModule()
{
    if (_logger != NULL)
    {
        destroyLogger(_logger);
    }
    if (_outputFile != NULL)
    {
        fclose(_outputFile);
    }
}

/** PRIVATE FUNCTIONS */

static int _itoa(uint64_t v, char *sp);
static void _generatePrologue(void);
static void _generateProgram(automaton *automaton);
static void _generateEpilogue(void);
static char *_indentation(const unsigned int indentationLevel);
static void _output(const unsigned int indentationLevel, const char *const format, ...);

/* JAVA FUNCTIONS */
// Leaf-level functions
char *_computeLiteral(Literal *literal);
char *_computeVarAccess(VarAccess *varAccess);
char *_computeArgumentList(ArgumentList *argumentList);
char *_computePostfixExpression(PostfixExpression *postfixExpression);
char *_computeUnaryExpression(UnaryExpression *unaryExpression);
char *_computeTypes(Type *type);

//----------------Unary Expression Aux functions -----------------------------------------
char *_computeNumericComparison(char *left, char *right, NumericComparison *numcomp);
char *_computeDoubleTokenExpression(char *left, char *right, UnaryExpressionType type);
char *_computeSingleTokenOperator(char *operand, Token token);
// ---------------------------------------------------------------------------------------

char *_computeEqualityExpression(EqualityExpression *equalityExpression);
char *_computeConditionalAndExpression(ConditionalAndExpression *conditionalAndExpression);
char *_computeConditionalOrExpression(ConditionalOrExpression *conditionalOrExpression);
char *_computeConditionalExpression(ConditionalExpression *conditionalExpression);
char *_computeAssignment(Assignment *assignment);
char *_computePrimary(Primary *primary);
char *_computeClassInstanceCreationExpression(ClassInstanceCreationExpression *classInstanceCreationExpression);
char *_computeUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression *unqualifiedClassInstanceCreationExpression);

// Mid-level functions
char *_computeExpression(Expression *expression);
char *_computeMethodInvocation(MethodInvocation *methodInvocation);
char *_computeStatementExpression(StatementExpression *statementExpression);
char *_computeStatementExpressionList(StatementExpressionList *statementExpressionList);
char *_computeIfThenStatement(IfThenStatement *ifThenStatement);
char *_computeForInit(ForInit *forInit);

// Higher-level functions
char *_computeStatement(Statement *statement);
char *_computeBlock(Block *block);

// Top-level function
char *_computeAction(Action *my_action);

// Definitions:
// Leaf-level functions
char *_computeTypes(Type *type)
{
    if (type == NULL)
    {
        return strdup("");
    }

    char *result = NULL;

    switch (type->stuff)
    {
    case STRING_TYPE:
        result = strdup("String");
        break;

    case INTEGER_TYPE:
        result = strdup("Integer");
        break;

    case DOUBLE_TYPE:
        result = strdup("Double");
        break;

    case BOOLEAN_TYPE:
        result = strdup("Boolean");
        break;

    case TOKEN_TYPE:
        result = strdup("Token");
        break;

    default:
        result = strdup("");
        break;
    }

    return result;
}

char *_computeLiteral(Literal *literal)
{
    if (literal == NULL)
    {
        return NULL;
    }

    if (literal->type == STRING_T)
    {
        size_t len = strlen(literal->str) + 3;
        char *stringinastring = malloc(sizeof(char) * len);
        snprintf(stringinastring, len, "\"%s\"", literal->str);
        return stringinastring;
    }
    else if (literal->type == TOKEN_T)
    {
        if(literal->token == JAVA_TRUE){
            return strdup("true");
        } else if(literal->token == JAVA_FALSE){
            return strdup("false");
        } else {
            char *tok = malloc(sizeof(char) * 10);
            snprintf(tok, 10, "%d", literal->token);
            return tok;
        }
    }
    else
    {
        return NULL;
    }
}

char *_computeVarAccess(VarAccess *varAccess)
{
    if (varAccess == NULL)
    {
        return NULL;
    }

    if (varAccess->vaccess != NULL)
    {

        // Case 3: Operator on VarAccess (e.g., var_name.VarAccess.var_name)

        if (varAccess->var_name != NULL)
        {
            char *nestedResult = _computeVarAccess(varAccess->vaccess);

            size_t len = strlen(nestedResult) + strlen(varAccess->var_name) + 2; // 2 for the dot and the null terminator
            char *result = calloc(len, sizeof(char));
            snprintf(result, len, "%s.%s", varAccess->var_name, nestedResult);

            free(nestedResult);

            return result;
        }

        // Case 4: Parameter-based access (e.g., par.VarAccess)

        else if (varAccess->type != NULL)
        {
            char *nestedResult = _computeVarAccess(varAccess->vaccess);
            char *types = _computeTypes(varAccess->type);

            size_t len = strlen(nestedResult) + strlen(types) + 2;
            char *result = calloc(len, sizeof(char));
            snprintf(result, len, "%s.%s", types, nestedResult);

            free(nestedResult);
            free(types);
            return result;
        }
        else
        {
            return NULL;
        }
    }

    // Case 1: Base case - Simple var_name
    if (varAccess->var_name != NULL)
    {
        return strdup(varAccess->var_name);
    }

    // Case 2: Method invocation (e.g., a.b.c.d.method())
    if (varAccess->method_invocation != NULL)
    {
        return _computeMethodInvocation(varAccess->method_invocation);
    }

    return NULL;
}

char *_computeArgumentList(ArgumentList *argumentList)
{
    // Base case: empty argument list
    if (argumentList == NULL)
    {
        return strdup("");
    }

    char *currentExpression = _computeExpression(argumentList->expression);

    // Base case: no more arguments
    if (argumentList->arglist == NULL)
    {
        return currentExpression;
    }

    // Recursive case: there are more arguments
    char *restOfList = _computeArgumentList(argumentList->arglist);

    size_t len = strlen(currentExpression) + strlen(restOfList) + 2; // 2 for the comma and null-terminator
    char *result = malloc(sizeof(char) * len);

    snprintf(result, len, "%s,%s", currentExpression, restOfList);

    free(currentExpression);
    free(restOfList);

    return result;
}

char *_computePostfixExpression(PostfixExpression *postfixExpression)
{
    if (postfixExpression == NULL)
    {
        return NULL;
    }

    char *result = NULL;

    if (postfixExpression->primary != NULL)
    {
        result = _computePrimary(postfixExpression->primary);
    }

    else if (postfixExpression->vaccess != NULL)
    {

        result = _computeVarAccess(postfixExpression->vaccess);

        if (postfixExpression->token == INCREMENT)
        {
            size_t len = strlen(result) + 3;
            char *newResult = malloc(len);

            snprintf(newResult, len, "%s++", result);

            free(result);
            result = newResult;
        }
        else if (postfixExpression->token == DECREMENT)
        {
            size_t len = strlen(result) + 3;
            char *newResult = malloc(len);

            snprintf(newResult, len, "%s--", result);

            free(result);
            result = newResult;
        }
    }

    return result;
}

char *_computeUnaryExpression(UnaryExpression *unaryExpression)
{
    if (unaryExpression == NULL)
    {
        return NULL;
    }

    char *result = NULL;

    switch (unaryExpression->globaltype)
    {
    case NUMERIC_COMPARISON:
    {
        char *left = _computeUnaryExpression(unaryExpression->num_comp_unary_exp1);
        char *right = _computePostfixExpression(unaryExpression->num_comp_unary_exp2);

        result = _computeNumericComparison(left, right, unaryExpression->numcomp);
        free(left);
        free(right);
    }
    break;

    case DOUBLE_TOKEN:
    {
        char *left = _computeUnaryExpression(unaryExpression->uexp_unary_expression1);
        char *right = _computePostfixExpression(unaryExpression->uexp_unary_expression2);
        result = _computeDoubleTokenExpression(left, right, unaryExpression->uexp_type);
        free(left);
        free(right);
    }
    break;

    case POSTFIX_EXPRESSION:
        result = _computePostfixExpression(unaryExpression->postfix_expression);
        break;

    case TYPE:
    {
        if (unaryExpression->object_type == NULL)
        {
            result = strdup("()");
        }
        else
        {
            char *types = _computeTypes(unaryExpression->object_type);
            size_t total = strlen(types) + 3;
            result = malloc(sizeof(char) * total);
            snprintf(result, total, "(%s)", types);
            free(types);
        }
    }
    break;

    case SINGLE_TOKEN:
    {
        char *operand = _computeUnaryExpression(unaryExpression->unary_expression);
        result = _computeSingleTokenOperator(operand, unaryExpression->token);
        free(operand);
    }
    break;

    default:
        result = NULL;
        break;
    }

    return result;
}

char *_computeNumericComparison(char *left, char *right, NumericComparison *numcomp)
{
    char *operator= NULL;
    switch (numcomp->token)
    {
    case JAVA_EXACT_COMPARISON:
        operator= "==";
        break;
    case JAVA_NOT_EXACT_COMPARISON:
        operator= "!=";
        break;
    case JAVA_LESSER:
        operator= "<";
        break;
    case JAVA_GREATER:
        operator= ">";
        break;
    case JAVA_LEQ:
        operator= "<=";
        break;
    case JAVA_GEQ:
        operator= ">=";
        break;
    default:
        operator= "unknown";
        break;
    }

    size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
    char *result = malloc(sizeof(char) * len);
    snprintf(result, len, "%s %s %s", left, operator, right);
    return result;
}

char *_computeDoubleTokenExpression(char *left, char *right, UnaryExpressionType type)
{
    char *operator= NULL;
    switch (type)
    {
    case STAR_TYPE:
        operator= "*";
        break;
    case DIV_TYPE:
        operator= "/";
        break;
    case MOD_TYPE:
        operator= "%";
        break;
    case PLUS_TYPE:
        operator= "+";
        break;
    case MINUS_TYPE:
        operator= "-";
        break;
    default:
        operator= "unknown";
        break;
    }

    size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
    char *result = malloc(sizeof(char) * len);
    snprintf(result, len, "%s %s %s", left, operator, right);
    return result;
}

char *_computeSingleTokenOperator(char *operand, Token token)
{
    char *operator= NULL;
    switch (token)
    {
    case JAVA_NOT:
        operator= "!";
        break;
    case INCREMENT:
        operator= "++";
        break;
    case DECREMENT:
        operator= "--";
        break;
    case PLUS:
        operator= "+";
        break;
    case MINUS:
        operator= "-";
        break;
    default:
        operator= "unknown";
        break;
    }

    size_t len = strlen(operand) + strlen(operator) + 2;
    char *result = malloc(sizeof(char) * len);
    snprintf(result, len, "%s%s", operator, operand);
    return result;
}

char *_computeEqualityExpression(EqualityExpression *equalityExpression)
{
    if (equalityExpression == NULL)
    {
        return NULL;
    }

    char *result = NULL;

    if (equalityExpression->equality_expression == NULL)
    {
        // Base case: Compute the unary expression (single UnaryExpression)
        result = _computeUnaryExpression(equalityExpression->unary_expression);
    }
    else
    {
        // Recursive case: Compute the left-hand side and right-hand side and combine with the operator
        char *left = _computeEqualityExpression(equalityExpression->equality_expression);
        char *right = _computeUnaryExpression(equalityExpression->unary_expression);

        const char *operator= equalityExpression->token == JAVA_EXACT_COMPARISON ? "==" : "!=";

        size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
        char *result = malloc(sizeof(char) * len);
        snprintf(result, len, "%s %s %s", left, operator, right);

        free(left);
        free(right);
    }

    return result;
}

char *_computeConditionalAndExpression(ConditionalAndExpression *conditionalAndExpression)
{
    if (conditionalAndExpression == NULL)
    {
        return NULL;
    }

    char *result = NULL;

    if (conditionalAndExpression->conditional_and_expression == NULL)
    {
        result = _computeEqualityExpression(conditionalAndExpression->equality_expression);
    }
    else
    {
        char *left = _computeConditionalAndExpression(conditionalAndExpression->conditional_and_expression);
        char *right = _computeEqualityExpression(conditionalAndExpression->equality_expression);

        const char *operator= "&&";

        size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
        result = malloc(sizeof(char) * len);
        snprintf(result, len, "%s %s %s", left, operator, right);

        free(left);
        free(right);
    }

    return result;
}

char *_computeConditionalOrExpression(ConditionalOrExpression *conditionalOrExpression)
{
    if (conditionalOrExpression == NULL)
    {
        return NULL;
    }

    char *result = NULL;

    if (conditionalOrExpression->conditional_or_expression == NULL)
    {
        result = _computeConditionalAndExpression(conditionalOrExpression->conditional_and_expression);
    }
    else
    {
        char *left = _computeConditionalOrExpression(conditionalOrExpression->conditional_or_expression);
        char *right = _computeConditionalAndExpression(conditionalOrExpression->conditional_and_expression);

        const char *operator= "||";

        size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
        result = malloc(sizeof(char) * len);
        snprintf(result, len, "%s %s %s", left, operator, right);

        free(left);
        free(right);
    }

    return result;
}

char *_computeConditionalExpression(ConditionalExpression *conditionalExpression)
{
    if (conditionalExpression == NULL)
    {
        return strdup("");
    }

    char *left = _computeConditionalOrExpression(conditionalExpression->conditional_or_expression);

    if (conditionalExpression->expression == NULL || conditionalExpression->conditional_expression == NULL)
    {
        return left;
    }

    // If there's both a middle expression and a right ConditionalExpression, combine them
    char *middle = _computeExpression(conditionalExpression->expression);
    char *right = _computeConditionalExpression(conditionalExpression->conditional_expression);
    size_t len = strlen(left) + strlen(middle) + strlen("?") + strlen(right) + strlen(":") + 5; // 5 for spaces and null terminator
    char *result = malloc(sizeof(char) * len);
    snprintf(result, len, "%s ? %s : %s", left, middle, right);

    // Clean up
    free(left);
    free(middle);
    free(right);

    return result;
}

char *_computeAssignment(Assignment *assignment)
{
    if (assignment == NULL)
    {
        return strdup("");
    }

    char *left = _computeVarAccess(assignment->vaccess);

    char *right = _computeExpression(assignment->expression);

    char *operator= NULL;
    switch (assignment->token)
    {
    case JAVA_ASSIGNMENT:
        operator= "=";
        break;
    case JAVA_PLUS_ASSIGN:
        operator= "+=";
        break;
    case JAVA_MINUS_ASSIGN:
        operator= "-=";
        break;
    case JAVA_MULTIPLY_ASSIGN:
        operator= "*=";
        break;
    case JAVA_DIVIDE_ASSIGN:
        operator= "/=";
        break;
    case JAVA_MODULO_ASSIGN:
        operator= "%=";
        break;
    case JAVA_LEFT_SHIFT_ASSIGN:
        operator= "<<=";
        break;
    case JAVA_RIGHT_SHIFT_ASSIGN:
        operator= ">>=";
        break;
    case JAVA_UNSIGNED_RIGHT_SHIFT_ASSIGN:
        operator= ">>>=";
        break;
    case JAVA_AND_ASSIGN:
        operator= "&=";
        break;
    case JAVA_XOR_ASSIGN:
        operator= "^=";
        break;
    case JAVA_OR_ASSIGN:
        operator= "|=";
        break;
    default:
        operator= "unknown";
        break;
    }

    size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;

    char *result = malloc(sizeof(char) * len);

    snprintf(result, len, "%s %s %s", left, operator, right);

    free(left);
    free(right);

    return result;
}

char *_computePrimary(Primary *primary)
{
    if (primary == NULL)
    {
        return strdup("");
    }

    char *result = NULL;

    switch (primary->type)
    {
    case LITERAL_TYPE:
        result = _computeLiteral(primary->literal);
        break;

    case EXPRESSION_TYPE:
        result = _computeExpression(primary->expression);
        break;

    case CONDITIONAL_EXPRESSION_TYPE:
        result = _computeClassInstanceCreationExpression(primary->class_inst_creation_exp);
        break;

    default:
        result = strdup("");
        break;
    }

    return result;
}

char *_computeClassInstanceCreationExpression(ClassInstanceCreationExpression *classInstanceCreationExpression)
{
    if (classInstanceCreationExpression == NULL)
    {
        return strdup("");
    }

    char *result = NULL;

    if (classInstanceCreationExpression->unq_class_inst_creation_exp != NULL && classInstanceCreationExpression->vaccess != NULL)
    {
        char *vaccessStr = _computeVarAccess(classInstanceCreationExpression->vaccess);
        char *uciceStr = _computeUnqualifiedClassInstanceCreationExpression(classInstanceCreationExpression->unq_class_inst_creation_exp);
        result = malloc(sizeof(char) * (strlen(vaccessStr) + strlen(uciceStr) + 2));
        sprintf(result, "%s.%s", vaccessStr, uciceStr);
        free(vaccessStr);
        free(uciceStr);
    }
    else if (classInstanceCreationExpression->unq_class_inst_creation_exp != NULL && classInstanceCreationExpression->primary != NULL)
    {
        char *primaryStr = _computePrimary(classInstanceCreationExpression->primary);
        char *uciceStr = _computeUnqualifiedClassInstanceCreationExpression(classInstanceCreationExpression->unq_class_inst_creation_exp);
        result = malloc(sizeof(char) * (strlen(primaryStr) + strlen(uciceStr) + 2));
        sprintf(result, "%s.%s", primaryStr, uciceStr);
        free(primaryStr);
        free(uciceStr);
    }
    else
    {
        result = _computeUnqualifiedClassInstanceCreationExpression(classInstanceCreationExpression->unq_class_inst_creation_exp);
    }

    return result;
}

char *_computeUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression *unqualifiedClassInstanceCreationExpression)
{
    if (unqualifiedClassInstanceCreationExpression == NULL)
    {
        return strdup("");
    }
    char *result = NULL;
    size_t total;
    if (unqualifiedClassInstanceCreationExpression->unq_type == PARARGS_TYPE)
    {
        char *arglistStr = _computeArgumentList(unqualifiedClassInstanceCreationExpression->arglist);
        char *types = _computeTypes(unqualifiedClassInstanceCreationExpression->type);
        total = strlen(arglistStr) + strlen(types) + 7;
        result = malloc(sizeof(char) * total);

        snprintf(result, total, "new %s(%s)", types, arglistStr);

        free(arglistStr);
        free(types);
    }
    else
    {
        char *methodStr = _computeMethodInvocation(unqualifiedClassInstanceCreationExpression->invocation);
        total = strlen(methodStr) + 5;
        result = malloc(sizeof(char) * total);
        snprintf(result, total, "new %s", methodStr);

        free(methodStr);
    }

    return result;
}

// Mid-level functions
char *_computeExpression(Expression *expression)
{
    if (expression == NULL)
    {
        return strdup("");
    }

    switch (expression->type)
    {
    case CONDITIONAL_EXP:
        return _computeConditionalExpression(expression->conditional_expression);

    case ASSIGNMENT_TYPE:
        return _computeAssignment(expression->assignment);

    default:
        return strdup("");
    }
}

char *_computeMethodInvocation(MethodInvocation *methodInvocation)
{
    if (methodInvocation == NULL)
    {
        return strdup("");
    }

    char *varAccessStr = _computeVarAccess(methodInvocation->vaccess);

    char *argumentListStr = _computeArgumentList(methodInvocation->arglist);

    size_t totalLength = strlen(varAccessStr) + strlen(argumentListStr) + 3;
    char *methodInvocationStr = malloc(sizeof(char) * totalLength);

    snprintf(methodInvocationStr, totalLength, "%s(%s)", varAccessStr, argumentListStr);

    free(varAccessStr);
    free(argumentListStr);

    return methodInvocationStr;
}

char *_computeStatementExpression(StatementExpression *statementExpression)
{
    if (statementExpression == NULL)
    {
        return strdup("");
    }

    char *result = NULL;

    switch (statementExpression->state_type)
    {
    case ASSIGNATION:
    {
        result = _computeAssignment(statementExpression->assignment);
        break;
    }
    case VAR_ACCESS:
    {
        result = _computeVarAccess(statementExpression->var_access);
        break;
    }
    case ASSIG_TYPE:
    {
        char *expStr = _computeExpression(statementExpression->expression);
        char *typeStr = _computeTypes(statementExpression->type);

        size_t totalLength = strlen(statementExpression->var_name) + strlen(expStr) + strlen(typeStr) + 5;
        result = malloc(sizeof(char) * totalLength);
        snprintf(result, totalLength, "%s %s = %s", typeStr, statementExpression->var_name, expStr);

        free(typeStr);
        free(expStr);
        break;
    }
    default:
        result = strdup("");
        break;
    }

    return result;
}

char *_computeStatementExpressionList(StatementExpressionList *statementExpressionList)
{
    if (statementExpressionList == NULL)
    {
        return strdup("");
    }

    char *result = NULL;
    char *currentExprStr = _computeStatementExpression(statementExpressionList->expression);

    if (statementExpressionList->expression_list == NULL)
    {
        result = currentExprStr;
    }
    else
    {
        char *restOfListStr = _computeStatementExpressionList(statementExpressionList->expression_list);

        size_t totalLength = strlen(currentExprStr) + strlen(restOfListStr) + 3;
        result = malloc(sizeof(char) * totalLength);
        snprintf(result, totalLength, "%s, %s", currentExprStr, restOfListStr);

        free(restOfListStr);
    }

    free(currentExprStr);

    return result;
}

char *_computeIfThenStatement(IfThenStatement *ifThenStatement)
{
    if (ifThenStatement == NULL)
    {
        return strdup("");
    }

    char *conditionStr = _computeExpression(ifThenStatement->expression);

    char *ifStatementStr = _computeBlock(ifThenStatement->if_block);

    char *result = malloc(sizeof(char) * (strlen("if () {  }") + strlen(conditionStr) + strlen(ifStatementStr) + 1));
    sprintf(result, "if (%s) { %s }", conditionStr, ifStatementStr);

    free(conditionStr);
    free(ifStatementStr);

    if (ifThenStatement->else_block != NULL)
    {
        char *elseStatementStr = _computeBlock(ifThenStatement->else_block);

        size_t totalLength = strlen(result) + strlen(" else { }") + strlen(elseStatementStr) + 3;

        char *newResult = malloc(totalLength);

        snprintf(newResult, totalLength, "%s else { %s }", result, elseStatementStr);

        free(result);
        result = newResult;

        free(elseStatementStr);
    }

    return result;
}

char *_computeForInit(ForInit *forInit)
{
    switch (forInit->for_type)
    {
    case STATEMENT_EXPRESSION_LIST:
    {
        return _computeStatementExpressionList(forInit->statement_expression_list);
    }

    case WITH_TYPES:
    {
        char *typeStr = _computeTypes(forInit->type);
        size_t totalLen = strlen(forInit->var_name_type) + strlen(typeStr) + 2;
        char *result = malloc(sizeof(char) * totalLen);
        snprintf(result, totalLen, "%s %s", typeStr, forInit->var_name_type);
        free(typeStr);
        return result;
    }

    case WITHOUT_TYPES:
    {
        return strdup(forInit->var_name);
    }

    default:
        return strdup("");
    }
}

// Higher-level functions
char *_computeStatement(Statement *statement)
{
    switch (statement->type)
    {
    case STATE_TYPE:
    {
        return _computeStatementExpression(statement->statement_expression);
    }

    case IF_THEN_STATEMENT:
    {
        return _computeIfThenStatement(statement->if_then_statement);
    }

    case WHILE_TYPE:
    {
        char *whileCondition = _computeExpression(statement->while_expression);
        char *whileStatement = _computeBlock(statement->while_block);

        size_t totalLen = strlen(whileCondition) + strlen(whileStatement) + 14;
        char *result = malloc(sizeof(char) * totalLen);
        snprintf(result, totalLen, "while (%s) { %s }", whileCondition, whileStatement);
        return result;
    }

    case FOR_TYPE:
    {
        char *forInit = _computeForInit(statement->for_init);
        char *forCondition = _computeExpression(statement->for_expression);
        char *forStatementList = _computeStatementExpressionList(statement->statement_expression_list);
        char *forBody = _computeBlock(statement->for_block);

        size_t totalLen = strlen(forInit) + strlen(forCondition) + strlen(forStatementList) + strlen(forBody) + 16;
        char *result = malloc(sizeof(char) * totalLen);
        if (result != NULL)
        {
            snprintf(result, totalLen, "for (%s; %s; %s) { %s }", forInit, forCondition, forStatementList, forBody);
        }
        return result;
    }

    default:
        return strdup("");
    }
}

char *_computeBlock(Block *block)
{
    switch (block->type)
    {
    case STATEMENT:
    {
        char *statementResult = _computeStatement(block->statement);
        char *nestedBlockResult = block->block != NULL ? _computeBlock(block->block) : NULL;
        size_t totalLen = strlen(statementResult) + (nestedBlockResult != NULL ? strlen(nestedBlockResult) : 0) + 10;
        char *result = malloc(sizeof(char) * totalLen);
        if (result != NULL)
        {
            snprintf(result, totalLen, "%s; %s", statementResult, nestedBlockResult != NULL ? nestedBlockResult : "");
        }
        free(statementResult);
        if (nestedBlockResult != NULL)
            free(nestedBlockResult);
        return result;
    }

    case RET:
    {
        char *returnExpr = _computeExpression(block->expression);
        size_t totalLen = strlen(returnExpr) + 9;
        char *result = malloc(sizeof(char) * totalLen);
        if (result != NULL)
        {
            snprintf(result, totalLen, "return %s;", returnExpr);
        }
        free(returnExpr);
        return result;
    }

    case THROW:
    {
        char *throwExpr = _computeExpression(block->expression);
        size_t totalLen = strlen(throwExpr) + 9;
        char *result = malloc(sizeof(char) * totalLen);
        if (result != NULL)
        {
            snprintf(result, totalLen, "throw %s;", throwExpr);
        }
        free(throwExpr);
        return result;
    }

    default:
        return strdup("");
    }
}

char *_computeAction(Action *my_action)
{
    if (my_action->type == ACTION_T)
    {
        return strdup(my_action->varName);
    }
    else if (my_action->type == FUNCTION_BODY)
    {
        char *block_str = _computeBlock(my_action->block);

        size_t totalLen = strlen(block_str) + 6;
        char *result = malloc(sizeof(char) * totalLen);
        snprintf(result, totalLen, "{ %s }", block_str);
        free(block_str);
        return result;
    }
    else
    {
        return strdup("IGNORE_FOR_NOW");
    }
}
/* END OF JAVA FUNCTIONS*/

// modified from https://stackoverflow.com/questions/3440726/what-is-the-proper-way-of-implementing-a-good-itoa-function
int _itoa(uint64_t v, char *sp)
{

    char tmp[MAX_UINT64_LENGTH]; // be careful with the length of the buffer
    char *tp = tmp;
    int i;

    while (v || tp == tmp)
    {
        i = v % 10;
        v /= 10;
        *tp++ = i + '0';
    }

    int len = tp - tmp;
    sp[len] = '\0';

    while (tp > tmp)
        *sp++ = *--tp;

    return len;
}

/**
 * Will not always write the output into the buffer, returned char* is the only valid return :P
 * bufferLength should be at least 6
 */
char *_escapeMatcher(char matcher, char *buffer, uint64_t bufferLength)
{
    switch (matcher)
    {
    case '\t':
        return "'\\t'";
    case '\n':
        return "'\\n'";
    case '\r':
        return "'\\r'";
    case '\'':
        return "'\\''";
    case '\\':
        return "'\\\\'";
    case '%':
        return "'%'";
    default:
        snprintf(buffer, bufferLength, "\'%c\'", matcher);
        return buffer;
    }
}

/**
 * Generates the output of the program.
 */
static void _generateProgram(automaton *automaton)
{
    char new_state_start[] = "/*%ld*/Automaton.newState(";
    char new_state_end[] = ");\n";
    char null[] = "(Function<StateTracker, Token>) null";
    char ignore[] = "var -> null";

    automaton_iterator *a_iterator = get_automaton_iterator(automaton);
    uint64_t index = 0;
    while (has_next_state(a_iterator))
    {
        _output(2, new_state_start, index++);
        automaton_state *s = get_next_state(a_iterator);
        if (throws_token(s))
        {
            Action *act = (Action *)get_token(s);
            if (act == NULL)
            {
                _output(0, ignore);
            }
            else
            {
                if (act->type == ACTION_T)
                {
                    _output(0, "var -> %s", act->varName);
                }
                else
                {
                    char *to_free = _computeAction(act);
                    _output(0, "var -> %s", to_free);
                    free(to_free);
                }
            }
        }
        else
        {
            _output(0, null);
        }
        _output(0, new_state_end);
    }
    free_automaton_iterator(a_iterator);
    _output(0, "\n\n");

    char set_transition_format[] = "Automaton.setTransition(%ld, %ld, %s);\n";
    char aux[MAX_UINT64_LENGTH];

    a_iterator = get_automaton_iterator(automaton);

    for (uint64_t state_index = 0; has_next_state(a_iterator); state_index++)
    {
        automaton_state *s = get_next_state(a_iterator);
        state_iterator *s_iterator = get_state_iterator(s);
        while (has_next_rule(s_iterator))
        {
            rule *r = get_next_rule(s_iterator);
            _output(2, set_transition_format, state_index, get_to_state_indices(r)[0], _escapeMatcher(get_transition_matcher(r), aux, MAX_UINT64_LENGTH));
        }
        free_state_iterator(s_iterator);
    }
    free_automaton_iterator(a_iterator);

    _output(0, "\n");
}

/**
 * Creates the prologue of the generated output, a Latex document that renders
 * a tree thanks to the Forest package.
 *
 * @see https://ctan.dcc.uchile.cl/graphics/pgf/contrib/forest/forest-doc.pdf
 */
static void _generatePrologue(void)
{
    _output(0, "%s",
            "//INSERT PACKAGE NAME HERE\n"
            "package Your_package;\n"
            "\n"
            "import java.util.*;\n"
            "import java.util.function.Function;\n"
            "\n"
            "//EDIT THIS IMPORT TO MATCH YOUR PACKAGE\n"
            "import static Your_package.Automaton.Token.*;\n"
            "\n"
            "public abstract class Automaton {\n"
            "\n"
            "    // EDIT THIS ENUM TO MATCH YOUR TOKENS\n"
            "    public enum Token {\n"
            "        PUT_YOUR_USED_TOKENS_HERE, UNKNOWN;\n"
            "\n"
            "        private String stringContent;\n"
            "        private Integer intContent;\n"
            "        private Boolean boolContent;\n"
            "        private Double doubleContent;\n"
            "        private Boolean hasParams = false;\n"
            "\n"
            "        public Token setStringContent(String stringContent) {\n"
            "            this.stringContent = stringContent;\n"
            "            this.hasParams = true;\n"
            "            return this;\n"
            "        }\n"
            "\n"
            "        public Token setIntContent(Integer intContent) {\n"
            "            this.intContent = intContent;\n"
            "            this.hasParams = true;\n"
            "            return this;\n"
            "        }\n"
            "\n"
            "        public Token setBoolContent(Boolean boolContent) {\n"
            "            this.boolContent = boolContent;\n"
            "            this.hasParams = true;\n"
            "            return this;\n"
            "        }\n"
            "\n"
            "        public Token setDoubleContent(Double doubleContent) {\n"
            "            this.doubleContent = doubleContent;\n"
            "            this.hasParams = true;\n"
            "            return this;\n"
            "        }\n"
            "\n"
            "        public String getStringContent() {\n"
            "            return stringContent;\n"
            "        }\n"
            "\n"
            "        public Integer getIntContent() {\n"
            "            return intContent;\n"
            "        }\n"
            "\n"
            "        public Boolean getBoolContent() {\n"
            "            return boolContent;\n"
            "        }\n"
            "\n"
            "        public Double getDoubleContent() {\n"
            "            return doubleContent;\n"
            "        }\n"
            "\n"
            "        @Override\n"
            "        public String toString() {\n"
            "            StringBuilder sb = new StringBuilder();\n"
            "            sb.append(this.name());\n"
            "            if (hasParams) {\n"
            "                sb.append(\"{ \");\n"
            "                if (this.stringContent != null) {\n"
            "                    sb.append(\"stringContent: \").append(this.stringContent).append(\" \");\n"
            "                }\n"
            "                if (this.intContent != null) {\n"
            "                    sb.append(\"intContent: \").append(this.intContent).append(\" \");\n"
            "                }\n"
            "                if (this.boolContent != null) {\n"
            "                    sb.append(\"boolContent: \").append(this.boolContent).append(\" \");\n"
            "                }\n"
            "                if (this.doubleContent != null) {\n"
            "                    sb.append(\"doubleContent: \").append(this.doubleContent).append(\" \");\n"
            "                }\n"
            "                sb.append(\"}\");\n"
            "            }\n"
            "            return sb.toString();\n"
            "        }\n"
            "    }\n"
            "\n"
            "    private static State initialState;\n"
            "    private static final List<State> states = new ArrayList<>();\n"
            "    private static StateTracker stateTracker = new StateTracker();\n"
            "\n"
            "    public static int newState(Token token) {\n"
            "        return newState((s) -> token);\n"
            "    }\n"
            "\n"
            "    public static int newState(Function<StateTracker, Token> tokenGenerator) {\n"
            "        State state = new State(tokenGenerator);\n"
            "        if (initialState == null)\n"
            "            initialState = state;\n"
            "        states.add(state);\n"
            "        return states.size() - 1;\n"
            "    }\n"
            "\n"
            "    public static void setInitialState(int index) {\n"
            "        initialState = states.get(index);\n"
            "    }\n"
            "\n"
            "    public static void setTransition(int from, int to, char symbol) {\n"
            "        states.get(from).setTransition(states.get(to), symbol);\n"
            "    }\n"
            "\n"
            "    private static void manageState(char symbol) {\n"
            "        Automaton.stateTracker.lexeme.append(symbol);\n"
            "        if (symbol == '\\n') {\n"
            "            Automaton.stateTracker.attribute.row++;\n"
            "            Automaton.stateTracker.attribute.column = 1;\n"
            "        } else {\n"
            "            Automaton.stateTracker.attribute.column++;\n"
            "        }\n"
            "    }\n"
            "\n"
            "    private static void foundTokenManageState(Token token) {\n"
            "        Automaton.stateTracker.lexeme = new StringBuilder();\n"
            "		if (token != null)\n"
            "	        Automaton.stateTracker.token = token;\n"
            "    }\n"
            "\n"
            "    private static void updateStateInRange(char[] charArray, int rangeStart, int rangeEnd) {\n"
            "        for (int i = rangeStart; i < rangeEnd; i++) {\n"
            "            manageState(charArray[i]);\n"
            "        }\n"
            "    }\n"
            "\n"
            "    private record TokenAndConsume(Token token, int consumeIndex) {\n"
            "    }\n"
            "\n"
            "    private static TokenAndConsume getNextToken(char[] charArray, int readIndex) {\n"
            "        State current = initialState;\n"
            "        Function<StateTracker, Token> foundToken = null;\n"
            "        int consumeIndex = readIndex, startIndex = readIndex;\n"
            "        while (current != null && readIndex < charArray.length) {\n"
            "            Function<StateTracker, Token> aux = current.tokenGenerator;\n"
            "            if (aux != null) {\n"
            "                foundToken = aux;\n"
            "                consumeIndex = readIndex;\n"
            "            }\n"
            "            current = current.getTransition(charArray[readIndex]);\n"
            "            readIndex++;\n"
            "        }\n"
            "        Function<StateTracker, Token> aux;\n"
            "        if (current != null && (aux = current.tokenGenerator) != null) {\n"
            "            foundToken = aux;\n"
            "            consumeIndex = readIndex;\n"
            "        }\n"
            "        updateStateInRange(charArray, startIndex, consumeIndex);\n"
            "        if (foundToken == null) {\n"
            "		 	throw new NoSuchElementException(\"Error on row \%d, from column \%d to column \%d: \%s\"\n"
            "                   .formatted(stateTracker.attribute.row, startIndex, consumeIndex, stateTracker.lexeme));\n"
            "        return new TokenAndConsume(foundToken.apply(stateTracker), consumeIndex);\n"
            "    }\n"
            "\n"
            "    public static List<Token> getTokenList(String s) {\n"
            "	 stateTracker = new StateTracker();\n"
            "        TokenAndConsume tokenAndConsume;\n"
            "        int i;\n"
            "        char[] chars = s.toCharArray();\n"
            "        List<Token> tokens = new ArrayList<>();\n"
            "        for (i = 0; i < chars.length;) {\n"
            "            tokenAndConsume = getNextToken(chars, i);\n"
            "            foundTokenManageState(tokenAndConsume.token);\n"
            "           if (tokenAndConsume.token != null)\n"
            "            	tokens.add(tokenAndConsume.token);\n"
            "            i = tokenAndConsume.consumeIndex;\n"
            "        }\n"
            "        return tokens;\n"
            "    }\n"
            "\n"
            "    public static class StateTracker {\n"
            "        private StringBuilder lexeme = new StringBuilder();\n"
            "        private Token token;\n"
            "        public final Attribute attribute = new Attribute();\n"
            "\n"
            "        public String getLexeme() {\n"
            "            return lexeme.toString();\n"
            "        }\n"
            "\n"
            "        public Token getToken() {\n"
            "            return token;\n"
            "        }\n"
            "\n"
            "        public Attribute getAttribute() {\n"
            "            return attribute;\n"
            "        }\n"
            "\n"
            "        public static class Attribute {\n"
            "            // User managed\n"
            "            public Integer id, num;\n"
            "            // Non-user managed\n"
            "            private Integer row = 0, column = 1;\n"
            "\n"
            "            public Integer getRow() {\n"
            "                return row;\n"
            "            }\n"
            "\n"
            "            public Integer getColumn() {\n"
            "                return column;\n"
            "            }\n"
            "        }\n"
            "    }\n"
            "\n"
            "    private static class State {\n"
            "        private final Map<Character, State> transitions;\n"
            "        private final Function<StateTracker, Token> tokenGenerator;\n"
            "\n"
            "        private State(Function<StateTracker, Token> tokenGenerator) {\n"
            "            this.tokenGenerator = tokenGenerator;\n"
            "            this.transitions = new HashMap<>();\n"
            "        }\n"
            "\n"
            "        public void setTransition(State to, char symbol) {\n"
            "            transitions.put(symbol, to);\n"
            "        }\n"
            "\n"
            "        public State getTransition(char symbol) {\n"
            "            return transitions.get(symbol);\n"
            "        }\n"
            "    }\n"
            "\n"
            "    private static boolean initialized = false;\n"
            "\n"
            "    public static void initialize() {\n"
            "        if (initialized)\n"
            "            throw new IllegalStateException();\n"
            "        initialized = true;\n");
}

/**
 * Creates the epilogue of the generated output, that is, the final lines that
 * completes a valid Latex document.
 */
static void _generateEpilogue()
{
    _output(0, "%s",
            "    }\n"
            "}");
}

/**
 * Generates an indentation string for the specified level.
 */
static char *_indentation(const unsigned int level)
{
    return indentation(_indentationCharacter, level, _indentationSize);
}

/**
 * Outputs a formatted string to standard output. The "fflush" instruction
 * allows to see the output even close to a failure, because it drops the
 * buffering.
 */
static void _output(const unsigned int indentationLevel, const char *const format, ...)
{
    va_list arguments;
    va_start(arguments, format);
    char *indentation = _indentation(indentationLevel);
    char *effectiveFormat = concatenate(2, indentation, format);
    if (_outputFile != NULL)
    {
        vfprintf(_outputFile, effectiveFormat, arguments);
        fflush(_outputFile);
    }
    else
    {
        vfprintf(stdout, effectiveFormat, arguments);
        fflush(stdout);
    }
    free(effectiveFormat);
    free(indentation);
    va_end(arguments);
}

/** PUBLIC FUNCTIONS */

void generate(CompilerState *compilerState)
{
    logDebugging(_logger, "Generating final output...");
    _generatePrologue();
    _generateProgram(compilerState->automaton);
    _generateEpilogue();
    logDebugging(_logger, "Generation is done.");
}
