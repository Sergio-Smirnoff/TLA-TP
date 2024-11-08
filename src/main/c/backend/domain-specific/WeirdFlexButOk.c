#include "WeirdFlexButOk.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* MODULE INTERNAL STATE */

static Logger *_logger = NULL;
static transformer_list *list;
static transformer_list *current = NULL;
static Valid_Regex_List *validRegexList;
static automaton *automat;
static ComputationResult *result;
static boolean has_default = false;

// should be deleted
static FILE *logFile;

/** PRIVATE FUNCTIONS */
static void _addToList(Lexeme_precursor *lexeme, char *returner);
static void _freeTransformerList(struct transformer_list *list);
static char *_strConcat(char *str1, char *str2);
void _ruleset(Ruleset *my_ruleset);
void _computeRule(Rule *my_rule);
void _regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex);
void _computeRegexClass(Regex_class *regexClass, uint64_t startIndex, uint64_t endIndex);
void _computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, char *returner, uint64_t currentIndex);
uint64_t _computeLexeme(Lexeme *lexeme, uint64_t currentIndex, char *returner, boolean isEndOfChain);

// Leaf-level functions
char *_computeLiteral(Literal* literal);
char *_computeVarAccess(VarAccess* varAccess);
char *_computeArgumentList(ArgumentList* argumentList);
char *_computePostfixExpression(PostfixExpression* postfixExpression);
char *_computeUnaryExpression(UnaryExpression* unaryExpression);
char *_computeParams(Param* params);

//----------------Unary Expression Aux functions -----------------------------------------
char* _computeNumericComparison(char *left, char *right, NumericComparison *numcomp);
char* _computeDoubleTokenExpression(char *left, char *right, UnaryExpressionType type);
char* _computeSingleTokenOperator(char *operand, Token token);
// ---------------------------------------------------------------------------------------

char *_computeEqualityExpression(EqualityExpression* equalityExpression);
char *_computeConditionalAndExpression(ConditionalAndExpression* conditionalAndExpression);
char *_computeConditionalOrExpression(ConditionalOrExpression* conditionalOrExpression);
char *_computeConditionalExpression(ConditionalExpression* conditionalExpression);
char *_computeAssignment(Assignment* assignment);
char *_computePrimary(Primary* primary);
char *_computeClassInstanceCreationExpression(ClassInstanceCreationExpression* classInstanceCreationExpression);
char *_computeUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression* unqualifiedClassInstanceCreationExpression);

// Mid-level functions
char *_computeExpression(Expression* expression);
char *_computeMethodInvocation(MethodInvocation* methodInvocation);
char *_computeStatementExpression(StatementExpression* statementExpression);
char *_computeStatementExpressionList(StatementExpressionList* statementExpressionList);
char *_computeIfThenStatement(IfThenStatement* ifThenStatement);
char *_computeForInit(ForInit* forInit);

// Higher-level functions
char *_computeStatement(Statement* statement);
char *_computeBlock(Block* block);

// Top-level function
char *_computeAction(Action* my_action);


static void _addToList(Lexeme_precursor *lexeme, char *returner)
{
    if (lexeme != NULL)
    {
        if (current != NULL)
        {
            current->next = (transformer_list *)calloc(1, sizeof(transformer_list));
            if (errno != 0)
            {
                return; // podriamos loggear el error
            }
            current = current->next;
        }
        else
        {
            current = list;
        }
        current->lexeme = lexeme;
        current->returner = returner;
        current->next = NULL;
    }
    else
    {
        fprintf(logFile, "Error: Lexeme is NULL. Bad built table.\n");
    }
}

// Seems useless
static void _freeTransformerList(struct transformer_list *list)
{
    transformer_list *aux = list;
    while (aux != NULL)
    {
        transformer_list *to_free = aux;
        aux = aux->next;
        if (to_free->returner != NULL)
        {
            free(to_free->returner);
        }
        free(to_free);
    }
}

static char *_strConcat(char *str1, char *str2)
{
    char *aux = calloc((strlen(str1) + strlen(str2) + 1), sizeof(char));
    sprintf(aux, "%s%s", str1, str2);
    return aux;
}

/** PUBLIC FUNCTIONS */

void initializeWeirdFlexModule()
{
    _logger = createLogger("Weird Flex");
    list = (struct transformer_list *)calloc(1, sizeof(struct transformer_list));
    logFile = fopen("Backend.log", "a");
}

void shutdownWeirdFlexModule()
{
    if (_logger != NULL)
    {
        destroyLogger(_logger);
    }
    fclose(logFile);
    _freeTransformerList(list);
}

ComputationResult *computeProgram(Program *tree, Valid_Regex_List *regexList)
{
    result = (ComputationResult *)calloc(1, sizeof(ComputationResult));
    validRegexList = regexList;

    _ruleset(tree->ruleset);
    if (list->lexeme == NULL)
    {
        result->succeed = false;
        result->errorMessage = strdup("No rules");
        return result;
    }
    result->succeed = true;
    result->list = list;
    return result;
}

void _ruleset(Ruleset *my_ruleset)
{
    if (my_ruleset == NULL)
    {
        return;
    }
    _computeRule(my_ruleset->rule);
    _ruleset(my_ruleset->ruleset);
}

void _computeRule(Rule *my_rule)
{
    if (my_rule == NULL)
    {
        return;
    }
    Lexeme_precursor *lexeme;
    char *returner;
    switch (my_rule->type)
    {
    case lexeme_action:
        lexeme = my_rule->lex;
        returner = _computeAction(my_rule->action);
        fprintf(logFile, "Action: %s\n", returner);
        fflush(logFile);
        _addToList(lexeme, returner);
        break;
    case ignore_lexeme:
        lexeme = my_rule->lexeme;
        returner = NULL;
        _addToList(lexeme, returner);
        break;
    case regex:
        Regexes *regex_content = my_rule->regexes;
        Valid_Regex_List_Node *aux = validRegexList->head;
        while (aux != NULL)
        {
            if (strcmp(aux->regex_id, my_rule->our_regex_id) == 0)
            {
                aux->regex = regex_content;
                return;
            }
            aux = aux->next;
        }
        break;
    }
}

// Leaf-level functions
char *_computeParams(Param* params){
    if(params == NULL){
        return strdup("");
    }

    char* result = NULL;

    switch(params->stuff){
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

char *_computeLiteral(Literal* literal){
    if(literal == NULL){
        return strdup("");
    }

    if(literal->type == str){
        return strdup(literal->str);
    } else if(literal->type == token){
        char * tok = malloc(10);
        snprintf(tok, 10, "%d", literal->token);
        return strdup(tok);
    } else {
        return strdup("");
    }
}

char* _computeVarAccess(VarAccess* varAccess) {
    if (varAccess == NULL) {
        return strdup("");
    }

    if (varAccess->vaccess != NULL) {

        // Case 3: Operator on VarAccess (e.g., var_name.VarAccess.var_name)

        if(varAccess->var_name != NULL){
            char* nestedResult = _computeVarAccess(varAccess->vaccess);

            size_t len = strlen(nestedResult) + strlen(varAccess->var_name) + 2;  //2 for the dot and the null terminator
            char* result = calloc(len, sizeof(char));
            snprintf(result, len, "%s.%s", varAccess->var_name, nestedResult);
            
            free(nestedResult);

            return result;
        }

    
        // Case 4: Parameter-based access (e.g., par.VarAccess)

        else if(varAccess->param != NULL){
            char* nestedResult = _computeVarAccess(varAccess->vaccess);
            char* params = _computeParams(varAccess->param);

            size_t len = strlen(nestedResult) + strlen(params) + 2;
            char* result = calloc(len, sizeof(char));
            snprintf(result, len, "%s.%s", params, nestedResult);

            free(nestedResult);
            free(params);
            return result;

        } else {
            return strdup("");
        }
    }

    // Case 1: Base case - Simple var_name
    if (varAccess->var_name != NULL) {
        return strdup(varAccess->var_name); 
    }

    // Case 2: Method invocation (e.g., a.b.c.d.method())
    if (varAccess->method_invocation != NULL) {
        char* methodResult = _computeMethodInvocation(varAccess->method_invocation);
        return methodResult;
    }

    return strdup("");
}

char *_computeArgumentList(ArgumentList* argumentList){
    // Base case: empty argument list
    if (argumentList == NULL) {
        return strdup("");
    }

    char *currentExpression = _computeExpression(argumentList->expression);

    // Base case: no more arguments
    if (argumentList->arglist == NULL) {
        return currentExpression;
    }

    //Recursive case: there are more arguments
    char *restOfList = _computeArgumentList(argumentList->arglist);

    size_t len = strlen(currentExpression) + strlen(restOfList) + 2;  // 2 for the comma and null-terminator
    char *result = malloc(len);

    snprintf(result, len, "%s,%s", currentExpression, restOfList);

    free(currentExpression);
    free(restOfList);

    return result;
}

char* _computePostfixExpression(PostfixExpression* postfixExpression) {
    if (postfixExpression == NULL) {
        return strdup("");
    }

    char *result = NULL;

    if (postfixExpression->primary != NULL) {
        result = _computePrimary(postfixExpression->primary);
    }

    else if (postfixExpression->vaccess != NULL) {
        result = _computeVarAccess(postfixExpression->vaccess);

        if (postfixExpression->token == INCREMENT) {
            size_t len = strlen(result) + 2;
            result = realloc(result, len);
            strcat(result, "++");
        } else if (postfixExpression->token == DECREMENT) {
            size_t len = strlen(result) + 2; 
            result = realloc(result, len);
            strcat(result, "--");
        }
    }

    return result;
}

char* _computeUnaryExpression(UnaryExpression* unaryExpression) {
    if (unaryExpression == NULL) {
        return strdup("");
    }

    char *result = NULL;
    switch (unaryExpression->globaltype) {
        case numericComparison:
            {
                char *left = _computeUnaryExpression(unaryExpression->uexp1_num);
                char *right = _computePostfixExpression(unaryExpression->uexp2_num);

                result = _computeNumericComparison(left, right, unaryExpression->numcomp);
                free(left);
                free(right);
            }
            break;

        case doubleToken:
            {
                char *left = _computeUnaryExpression(unaryExpression->uexp1_exp);
                char *right = _computePostfixExpression(unaryExpression->uexp2_exp);
                result = _computeDoubleTokenExpression(left, right, unaryExpression->type);
                free(left);
                free(right);
            }
            break;

        case postfixExpression:
            result = _computePostfixExpression(unaryExpression->pexp);
            break;

        case param:
            {
                if(unaryExpression->param == NULL){
                    result = strdup("()");
                } else {
                    char* params= _computeParams(unaryExpression->param);
                    size_t total = strlen(params) + 3;
                    result = malloc(total);
                    snprintf(result, total, "(%s)", params);
                    free(params);
                }
            }
            break;

        case singleToken:
            {
                char *operand = _computeUnaryExpression(unaryExpression->uexp);
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

char* _computeNumericComparison(char *left, char *right, NumericComparison *numcomp) {
    char *operator = NULL;
    switch (numcomp->token) {
        case JAVA_EXACT_COMPARISON: operator = "=="; break;
        case JAVA_LESSER: operator = "<"; break;
        case JAVA_GREATER: operator = ">"; break;
        case JAVA_LEQ: operator = "<="; break;
        case JAVA_GEQ: operator = ">="; break;
        default: operator = "unknown"; break;
    }

    size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
    char *result = malloc(len);
    snprintf(result, len, "%s %s %s", left, operator, right);
    return result;
}


char* _computeDoubleTokenExpression(char *left, char *right, UnaryExpressionType type) {
    char *operator = NULL;
    switch (type) {
        case star_t: operator = "*"; break;
        case div_type: operator = "/"; break;
        case mod_t: operator = "%"; break;
        case plus_t: operator = "+"; break;
        case minus_t: operator = "-"; break;
        default: operator = "unknown"; break;
    }

    size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
    char *result = malloc(len);
    snprintf(result, len, "%s %s %s", left, operator, right);
    return result;
}


char* _computeSingleTokenOperator(char *operand, Token token) {
    char *operator = NULL;
    switch (token) {
        case JAVA_NOT: operator = "!"; break;
        case INCREMENT: operator = "++"; break;
        case DECREMENT: operator = "--"; break;
        case PLUS: operator = "+"; break;
        case MINUS: operator = "-"; break;
        default: operator = "unknown"; break;
    }

    size_t len = strlen(operand) + strlen(operator) + 2; 
    char *result = malloc(len);
    snprintf(result, len, "%s%s", operator, operand);
    return result;
}


char* _computeEqualityExpression(EqualityExpression* equalityExpression) {
    if (equalityExpression == NULL) {
        return strdup("");
    }

    char *result = NULL;

    if (equalityExpression->eqexp == NULL) {
        
        // Base case: Compute the unary expression (single UnaryExpression)
        result = _computeUnaryExpression(equalityExpression->uexp);
    } else {
        // Recursive case: Compute the left-hand side and right-hand side and combine with the operator
        char *left = _computeEqualityExpression(equalityExpression->eqexp);
        char *right = _computeUnaryExpression(equalityExpression->uexp);
        
        const char* operator = "==";

        size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
        char *result = malloc(len);
        snprintf(result, len, "%s %s %s", left, operator, right);

        free(left);
        free(right);
    }

    return result;
}

char* _computeConditionalAndExpression(ConditionalAndExpression* conditionalAndExpression) {
    if (conditionalAndExpression == NULL) {
        return strdup("");
    }

    char *result = NULL;
    if (conditionalAndExpression->candexp == NULL) {
        result = _computeEqualityExpression(conditionalAndExpression->eqexp);
    } else {
        char *left = _computeConditionalAndExpression(conditionalAndExpression->candexp); 
        char *right = _computeEqualityExpression(conditionalAndExpression->eqexp); 
        
        const char* operator = "&&";

        size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
        char *result = malloc(len);
        snprintf(result, len, "%s %s %s", left, operator, right);

        free(left);
        free(right);
    }

    return result;
}

char *_computeConditionalOrExpression(ConditionalOrExpression* conditionalOrExpression){
        if (conditionalOrExpression == NULL) {
        return strdup("");
    }

    char *result = NULL;

    if (conditionalOrExpression->corexp == NULL) {
        
        result = _computeConditionalAndExpression(conditionalOrExpression->candexp);
    } else {
        char *left = _computeConditionalOrExpression(conditionalOrExpression->corexp); 
        char *right = _computeConditionalAndExpression(conditionalOrExpression->candexp); 
        
        const char* operator = "||";

        size_t len = strlen(left) + strlen(right) + strlen(operator) + 3;
        char *result = malloc(len);
        snprintf(result, len, "%s %s %s", left, operator, right);

        free(left);
        free(right);
    }

    return result;
}

char* _computeConditionalExpression(ConditionalExpression* conditionalExpression) {
    if (conditionalExpression == NULL) {
        return strdup("");
    }
    char* left = _computeConditionalOrExpression(conditionalExpression->corexp);
    if (conditionalExpression->exp == NULL || conditionalExpression->cexp == NULL) {
        return left;
    }

    // If there's both a middle expression and a right ConditionalExpression, combine them
    char* middle = _computeExpression(conditionalExpression->exp);
    char* right = _computeConditionalExpression(conditionalExpression->cexp);
    size_t len = strlen(left) + strlen(middle) + strlen("?") + strlen(right) + strlen(":") + 5; // 5 for spaces and null terminator
    char* result = malloc(len);
    snprintf(result, len, "%s ? %s : %s", left, middle, right);
    
    // Clean up
    free(left);
    free(middle);
    free(right);

    return result;
}

char* _computeAssignment(Assignment* assignment) {
    if (assignment == NULL) {
        return strdup("");
    }

    char* left = _computeVarAccess(assignment->vaccess);

    char* right = _computeExpression(assignment->expression); 

    size_t len = strlen(left) + strlen(right) + strlen("=") + 3;

    char* result = malloc(len);

    snprintf(result, len, "%s = %s", left, right);

    free(left);
    free(right);

    return result;
}

char* _computePrimary(Primary* primary) {
    if (primary == NULL) {
        return strdup("");
    }

    char* result = NULL;

    switch (primary->type) {
        case literal:
            result = _computeLiteral(primary->lit);
            break;
        
        case expression:
            result = _computeExpression(primary->exp);
            break;

        case cexp:
            result = _computeClassInstanceCreationExpression(primary->cice);
            break;
        
        default:
            result = strdup("");
            break;
    }

    return result;
}


char* _computeClassInstanceCreationExpression(ClassInstanceCreationExpression* classInstanceCreationExpression) {
    if (classInstanceCreationExpression == NULL) {
        return strdup("");
    }

    char* result = NULL;
    
    if (classInstanceCreationExpression->ucice != NULL && classInstanceCreationExpression->vaccess != NULL) {
        char* vaccessStr = _computeVarAccess(classInstanceCreationExpression->vaccess);
        char* uciceStr = _computeUnqualifiedClassInstanceCreationExpression(classInstanceCreationExpression->ucice);
        result = malloc(strlen(vaccessStr) + strlen(uciceStr) + 2);
        sprintf(result, "%s.%s", vaccessStr, uciceStr);
        free(vaccessStr);
        free(uciceStr);

    } else if (classInstanceCreationExpression->ucice != NULL && classInstanceCreationExpression->primary != NULL) {
        char* primaryStr = _computePrimary(classInstanceCreationExpression->primary); 
        char* uciceStr = _computeUnqualifiedClassInstanceCreationExpression(classInstanceCreationExpression->ucice);
        result = malloc(strlen(primaryStr) + strlen(uciceStr) + 2);
        sprintf(result, "%s.%s", primaryStr, uciceStr);
        free(primaryStr);
        free(uciceStr);

    } else {
        result = _computeUnqualifiedClassInstanceCreationExpression(classInstanceCreationExpression->ucice); 
    }

    return result;
}


char* _computeUnqualifiedClassInstanceCreationExpression(UnqualifiedClassInstanceCreationExpression* unqualifiedClassInstanceCreationExpression) {
    if (unqualifiedClassInstanceCreationExpression == NULL) {
        return strdup("");
    }

    char* arglistStr = _computeArgumentList(unqualifiedClassInstanceCreationExpression->arglist);
    char* params = _computeParams(unqualifiedClassInstanceCreationExpression->param);
    size_t total = strlen(arglistStr) + strlen(params) + 7;
    char* result = malloc(total);

    snprintf(result, total, "new %s(%s)", params, arglistStr);

    free(arglistStr);
    free(params);

    return result;
}

// Mid-level functions
char* _computeExpression(Expression* expression) {
    if (expression == NULL) {
        return strdup("");
    }

    switch (expression->type) {
        case xexp:
            return _computeConditionalExpression(expression->xexp);

        case assignment:
            return _computeAssignment(expression->assignment);

        default:
            return strdup("");
    }
}


char* _computeMethodInvocation(MethodInvocation* methodInvocation) {
    if (methodInvocation == NULL) {
        return strdup("");
    }

    char* varAccessStr = _computeVarAccess(methodInvocation->vaccess);

    char* argumentListStr = _computeArgumentList(methodInvocation->arglist);

    size_t totalLength = strlen(varAccessStr) + strlen(argumentListStr) + 3;
    char* methodInvocationStr = malloc(totalLength);

    snprintf(methodInvocationStr, totalLength, "%s(%s)", varAccessStr, argumentListStr);

    free(varAccessStr);
    free(argumentListStr);

    return methodInvocationStr;
}


char* _computeStatementExpression(StatementExpression* statementExpression) {
    if (statementExpression == NULL) {
        return strdup("");
    }

    char* result = NULL;

    switch (statementExpression->type) {
        case assignation: {
            result = _computeAssignment(statementExpression->assignment);
            break;
        }
        case invocation: {
            result = _computeMethodInvocation(statementExpression->method_invocation);
            break;
        }
        case assigParam: {
            char* expStr = _computeExpression(statementExpression->exp);
            char* paramStr = _computeParams(statementExpression->param);
            size_t totalLength = strlen(statementExpression->var_name) + strlen(expStr) + strlen(paramStr) + 5;
            result = malloc(totalLength);
            snprintf(result, totalLength, "%s %s = %s", paramStr, statementExpression->var_name, expStr);

            free(paramStr);
            free(expStr);
            break;
        }
        default:
            result = strdup("");
            break;
    }

    return result;
}


char* _computeStatementExpressionList(StatementExpressionList* statementExpressionList) {
    if (statementExpressionList == NULL) {
        return strdup("");
    }

    char* result = NULL;
    char* currentExprStr = _computeStatementExpression(statementExpressionList->exp);

    if (statementExpressionList->list == NULL) {
        result = currentExprStr;
    } else {
        char* restOfListStr = _computeStatementExpressionList(statementExpressionList->list);

        size_t totalLength = strlen(currentExprStr) + strlen(restOfListStr) + 3;
        result = malloc(totalLength);
        snprintf(result, totalLength, "%s, %s", currentExprStr, restOfListStr);

        free(restOfListStr);
    }

    free(currentExprStr);

    return result;
}


char* _computeIfThenStatement(IfThenStatement* ifThenStatement) {
    if(ifThenStatement == NULL){
        return strdup("");
    }

    char* conditionStr = _computeExpression(ifThenStatement->exp);
    
    char* ifStatementStr = _computeStatement(ifThenStatement->statement1);
    
    char* result = malloc(strlen("if () {  }") + strlen(conditionStr) + strlen(ifStatementStr) + 1);
    sprintf(result, "if (%s) { %s }", conditionStr, ifStatementStr);
    
    free(conditionStr);
    free(ifStatementStr);
    
    if (ifThenStatement->statement2 != NULL) {
        char* elseStatementStr = _computeStatement(ifThenStatement->statement2);
        size_t totalLength = strlen(result) + strlen(" else { }") + strlen(elseStatementStr) + 1;
        
        result = realloc(result, totalLength);
        strcat(result, " else { ");
        strcat(result, elseStatementStr);
        strcat(result, " }");
        
        free(elseStatementStr);
    }
    
    return result;
}


char *_computeForInit(ForInit* forInit) {
    switch (forInit->type) {
        case statementExpList: {
            return _computeStatementExpressionList(forInit->statementExpList);
        }

        case withParams: {
            char *paramStr = _computeParams(forInit->param);
            size_t totalLen = strlen(forInit->var_name_param) + strlen(paramStr) + 2;
            char* result = malloc(totalLen);
            snprintf(result, totalLen, "%s %s", paramStr, forInit->var_name_param);
            free(paramStr);
            return result;
        }

        case withoutParams: {
            return strdup(forInit->var_name);
        }

        default:
             return strdup("");
    }
}

// Higher-level functions
char *_computeStatement(Statement* statement) {
    switch (statement->type) {
        case state: {
            return _computeStatementExpression(statement->sexp);
        }

        case ifThenStatement: {
            return _computeIfThenStatement(statement->ifThen);
        }

        case While: {
            char* whileCondition = _computeExpression(statement->expwhile);
            char* whileStatement = _computeStatement(statement->statementwhile);

            size_t totalLen = strlen(whileCondition) + strlen(whileStatement) + 14;
            char* result = malloc(totalLen);
            snprintf(result, totalLen, "while (%s) { %s }", whileCondition, whileStatement);
            return result;
        }

        case For: {
            char* forInit = _computeForInit(statement->forInit);
            char* forCondition = _computeExpression(statement->expfor);
            char* forStatementList = _computeStatementExpressionList(statement->statementExpList);
            char* forBody = _computeStatement(statement->statementfor);

            size_t totalLen = strlen(forInit) + strlen(forCondition) + strlen(forStatementList) + strlen(forBody) + 16;
            char* result = malloc(totalLen);
            if (result != NULL) {
                snprintf(result, totalLen, "for (%s; %s; %s) { %s }", forInit, forCondition, forStatementList, forBody);
            }
            return result;
        }

        default:
            return strdup("");
    }
}

char *_computeBlock(Block* block) {
    switch (block->type) {
        case statement: {
            char* statementResult = _computeStatement(block->statement);
            char* nestedBlockResult = block->block != NULL ? _computeBlock(block->block) : NULL;
            size_t totalLen = strlen(statementResult) + (nestedBlockResult != NULL ? strlen(nestedBlockResult) : 0) + 10;
            char* result = malloc(totalLen);
            if (result != NULL) {
                snprintf(result, totalLen, "%s; %s", statementResult, nestedBlockResult != NULL ? nestedBlockResult : "");
            }
            return result;
        }

        case ret: {
            char* returnExpr = _computeExpression(block->exp);
            size_t totalLen = strlen(returnExpr) + 9;
            char* result = malloc(totalLen);
            if (result != NULL) {
                snprintf(result, totalLen, "return %s;", returnExpr);
            }
            return result;
        }

        case throw: {
            char* throwExpr = _computeExpression(block->exp);
            size_t totalLen = strlen(throwExpr) + 9;
            char* result = malloc(totalLen);
            if (result != NULL) {
                snprintf(result, totalLen, "throw %s;", throwExpr);
            }
            return result;
        }

        default:
            return strdup("");
    }
}

char *_computeAction(Action *my_action)
{
    if (my_action->type == action)
    {
        return strdup(my_action->varName);
    }
    else if(my_action->type == function_body)
    {
        char *block_str = _computeBlock(my_action->block);
        char *params = _computeParams(my_action->param);

        size_t totalLen = strlen(block_str) + strlen(params) + 6;
        char * result = malloc(totalLen);
        snprintf(result, totalLen, "{ %s %s }", params, block_str);
        free(block_str);
        free(params);
        return result;
    } else {
        return strdup("IGNORE_FOR_NOW");
    }
    // Here it is the correct code:
    /*
    char *returner = calloc(1, sizeof(char));
    switch (my_action->type)
    {
    case action:
        returner->type = RETURN_STRING;
        returner->string = my_action->varName;
        break;
    case function_body:
        returner->type = JAVA_BLOCK;
        if (my_action->param == NULL)
        {
            returner->parameters = 0;
            returner->java_block = my_action->block;
        }
        else
        {
            returner->parameters = my_action->param->stuff;
            returner->java_block = my_action->block;
        }
    }
    return returner;
    */
}

void buildAutomaton(ComputationResult *computationResult)
{
    transformer_list *aux = computationResult->list;
    result = computationResult;
    automat = new_automaton();
    set_initial_state(automat, get_state(automat, new_state(automat, 0, NULL)));
    int i = 1;
    while (aux != NULL)
    {
        if (result->succeed == false)
        {
            return;
        }
        if (aux->lexeme != NULL)
        {
            _computeLexemePrecursor(aux->lexeme, aux->returner, 0);
        }
        else
        {
            computationResult->succeed = false;
            computationResult->errorMessage = strdup("Lexeme is NULL. Bad built table.");
            return;
        }
        aux = aux->next;
    }
    automaton *dfa = get_deterministic_equivalent(automat);
    free(automat);
    computationResult->automaton = dfa;
    return;
}

void _computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, char *returner, uint64_t currentIndex)
{
    if (lexeme_precursor == NULL)
    {
        fprintf(logFile, "You shouldn't be here mate\n");
        return;
    }
    switch (lexeme_precursor->precursor_type)
    {
    case literals:
        if (lexeme_precursor->type == default_lexeme)
        {
            if (has_default)
            {
                result->succeed = false;
                result->errorMessage = strdup("There can't be more than one default lexeme");
                return;
            }
            else
            {
                has_default = true;
                uint64_t defaultStateIndex = returner == NULL ? 0 : new_state(automat, 1, returner);
                for (unsigned char c = 9; c < 127; c++)
                {
                    if(!(c == 11 || c == 12 || (c >= 14 && c <= 31)))
                    {
                        set_transition(automat, 0, defaultStateIndex, c);
                    }
                }
                return;
            }
        }
        char *s = lexeme_precursor->string;
        uint64_t currentStateIndex = currentIndex;
        uint64_t nextStateIndex;
        while (*s)
        {
            if (!s[1])
            {
                if (returner == NULL)
                {
                    nextStateIndex = 0;
                }
                else
                {
                    nextStateIndex = new_state(automat, 1, returner);
                }
                set_transition(automat, currentStateIndex, nextStateIndex, *s);
            }
            else
            {
                nextStateIndex = new_state(automat, 0, NULL);
                set_transition(automat, currentStateIndex, nextStateIndex, *s);
            }
            s++;
            currentStateIndex = nextStateIndex;
        }
        return;
    case nonliterals:
        if (lexeme_precursor->lex_prec == NULL)
        {
            _computeLexeme(lexeme_precursor->lex, currentIndex, returner, 1);
        }
        else
        {
            uint64_t finalState = _computeLexeme(lexeme_precursor->lex, currentIndex, NULL, 0);
            _computeLexemePrecursor(lexeme_precursor->lex_prec, returner, finalState);
        }
        return;
    }
}

uint64_t _computeLexeme(Lexeme *lexeme, uint64_t currentIndex, char *returner, boolean isEndOfChain)
{
    uint64_t finalState = currentIndex;
    Regexes *node;
    switch (lexeme->type)
    {
    case regexes:
        node = lexeme->regexes;
        break;
    case name:
        Valid_Regex_List_Node *aux2 = validRegexList->head;
        while (aux2 != NULL)
        {
            if (strcmp(aux2->regex_id, lexeme->our_regex_id) == 0)
            {
                node = aux2->regex;
                break;
            }
            aux2 = aux2->next;
        }
        break;
    }
    if (lexeme->closure == NULL)
    {
        if(isEndOfChain)
        {
            finalState = new_state(automat, 1, returner);
        }
        else
        {
            finalState = new_state(automat, 0, NULL);
        }
        _regexContent(node, currentIndex, finalState);
    }
    else
    {
        if (lexeme->closure->closure == PLUS)
        {
            if(isEndOfChain)
            {
                finalState = new_state(automat, 1, returner);
            }
            else
            {
                finalState = new_state(automat, 0, NULL);
            }
            _regexContent(node, currentIndex, finalState);
        }
        _regexContent(node, finalState, finalState);
    }

    return finalState;
}

void _regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex)
{
    if (regexes->regexes == NULL)
    {
        _computeRegexClass(regexes->regexClass, startIndex, endIndex);
    }
    else
    {
        _computeRegexClass(regexes->regexClass, startIndex, endIndex);
        _regexContent(regexes->regexes, startIndex, endIndex);
        return;
    }
}

void _computeRegexClass(Regex_class *regexClass, uint64_t startIndex, uint64_t endIndex)
{
    if (regexClass == NULL)
    {
        return;
    }
    switch (regexClass->type)
    {
    case symbol:
        set_transition(automat, startIndex, endIndex, regexClass->symbol->symbol_tok[0]);
        return;
    case range:
        if (regexClass->startSymbol->symbol_tok[0] > regexClass->endSymbol->symbol_tok[0])
        {
            char *aux = _strConcat(regexClass->startSymbol->symbol_tok, "-");
            char *range = _strConcat(aux, regexClass->endSymbol->symbol_tok);
            char *to_print = _strConcat("Invalid range: ", range);
            result->succeed = false;
            result->errorMessage = to_print;
            free(range);
            free(aux);
            return;
        }
        else
        {
            for (unsigned char c = regexClass->startSymbol->symbol_tok[0]; c <= regexClass->endSymbol->symbol_tok[0]; c++)
            {
                set_transition(automat, startIndex, endIndex, c);
            }
            return;
        }
    case variable:
        Valid_Regex_List_Node *aux2 = validRegexList->head;
        while (aux2 != NULL)
        {
            if (strcmp(aux2->regex_id, regexClass->varName) == 0)
            {
                _regexContent(aux2->regex, startIndex, endIndex);
                break;
            }
            aux2 = aux2->next;
        }
        if (regexClass->closure != NULL)
        {
            fprintf(logFile, "You shouldn't be here mate\n");
        }
        return;
    }
}
