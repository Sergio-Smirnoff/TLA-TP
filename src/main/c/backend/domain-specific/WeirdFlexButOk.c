#include "WeirdFlexButOk.h"

#define TEMP_TOKEN 1

/* MODULE INTERNAL STATE */

static Logger *_logger = NULL;
static transformer_list *list;
static transformer_list *current;
static Valid_Regex_List *validRegexList;
static boolean has_default = false;
static FILE *logFile;

/** PRIVATE FUNCTIONS */
static void _addToList(Lexeme_precursor *lexeme, return_struct *returner);
static void _freeTransformerList(struct transformer_list *list);
static char *_strConcat(char *str1, char *str2);
void ruleset(Ruleset *my_ruleset);
void computeRule(Rule *my_rule);
uint64_t regexContent(Regexes *regexes, automaton *automaton, uint64_t currentIndex, boolean isStar);
char *computeRegexClass(Regex_class *regexClass);
void computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, automaton *automaton, return_struct *returner, uint64_t currentIndex);
uint64_t computeLexeme(Lexeme *lexeme, automaton *automaton, uint64_t currentIndex, return_struct *returner, boolean isEndOfChain);
return_struct *computeAction(Action *my_action);

static void _addToList(Lexeme_precursor *lexeme, return_struct *returner)
{
    if (lexeme != NULL)
    {
        current->next = (transformer_list *)calloc(1, sizeof(transformer_list));
        if (errno != 0)
        {
            return; // podriamos loggear el error
        }
        current = current->next;
    }

    current->lexeme = lexeme;
    current->returner = returner;
    current->next = NULL;
}

static void _freeTransformerList(struct transformer_list *list)
{
    transformer_list *aux = list;
    while (aux != NULL)
    {
        transformer_list *to_free = aux;
        aux = aux->next;
        if (to_free->lexeme != NULL)
        {
            free(to_free->lexeme);
        }
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
    current = list;
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

automaton *buildAutomaton(transformer_list *list)
{
    transformer_list *aux = list;
    automaton *automaton = new_automaton();
    set_initial_state(automaton, get_state(automaton, new_state(automaton, 0, 0)));
    while (aux != NULL)
    {
        if (aux->lexeme != NULL)
        {
            computeLexemePrecursor(aux->lexeme, automaton, aux->returner, 0);
        }
        else
        {
            perror("Error: Lexeme is NULL. Bad built table.");
        }
        // if (aux->returner != NULL){
        //     switch (aux->returner->type){
        //         case RETURN_STRING:
        //             printf("Returner: %s\n", aux->returner->string);
        //             break;
        //         case JAVA_BLOCK:
        //             if(aux->returner->parameters != 0){
        //                 printf("Returner param: %d\n", aux->returner->parameters);
        //             }
        //             printf("Returner block: %p\n", aux->returner->java_block);
        //             break;
        //         case RETURN_TOKEN:
        //             printf("Returner: %ls\n", aux->returner->token);
        //             break;
        //     }
        // }
        aux = aux->next;
    }
}

ComputationResult *computeProgram(Program *tree, Valid_Regex_List *regexList)
{
    ComputationResult *result = (ComputationResult *)calloc(1, sizeof(ComputationResult));
    validRegexList = regexList;

    ruleset(tree->ruleset);
    if (list == NULL)
    {
        result->succeed = false;
        return result;
    }
    result->succeed = true;
    result->value = list;
    return result;
}

void ruleset(Ruleset *my_ruleset)
{
    if (my_ruleset == NULL)
    {
        return;
    }
    computeRule(my_ruleset->rule);
    ruleset(my_ruleset->ruleset);
}

void computeRule(Rule *my_rule)
{
    if (my_rule == NULL)
    {
        return;
    }
    switch (my_rule->type)
    {
        Lexeme_precursor *lexeme;
        return_struct *returner;
    case lexeme_action:
        lexeme = my_rule->lex;
        returner = computeAction(my_rule->action);
        fprintf(logFile, "Did i compute?\n");
        fflush(logFile);
        _addToList(lexeme, returner);
        fprintf(logFile, "YES?\n");
        fflush(logFile);
        break;
    case ignore_lexeme:
        lexeme = my_rule->lexeme;
        returner = NULL;
        _addToList(lexeme, returner);
        break;
    case regex: // para mi no hace falta hacer esto
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

uint64_t regexContent(Regexes *regexes, automaton *automaton, uint64_t currentIndex, boolean isStar)
{
    if (regexes->regexes == NULL)
    {
        fprintf(logFile, "IM NULL\n");
        fflush(logFile);
        return computeRegexClass(regexes->regexClass);
    }
    else
    {
        fprintf(logFile, "IM CHAD NOT NULL\n");
        fflush(logFile);
        char *regex_class = computeRegexClass(regexes->regexClass);
        char *regex_content = regexContent(regexes->regexes);
        char *aux = _strConcat(regex_class, regex_content); // problema con mallocs, posible solucion
        free(regex_class);
        free(regex_content);
        return aux;
    }
}

char *computeRegexClass(Regex_class *regexClass)
{
    if (regexClass == NULL)
    {
        fprintf(logFile, "IM WEAK\n");
        fflush(logFile);
        return "";
    }
    char *aux;
    switch (regexClass->type)
    {
    case symbol:
        return regexClass->symbol->symbol_tok;
    case range:
        aux = malloc(4 * sizeof(char));
        aux[0] = regexClass->startSymbol->symbol_tok[0];
        aux[1] = '-';
        aux[2] = regexClass->endSymbol->symbol_tok[0];
        aux[3] = '\0';
        return aux;
    case variable: // @Patrick es por acá
        Valid_Regex_List_Node *aux2 = validRegexList->head;
        while (aux2 != NULL)
        {
            if (strcmp(aux2->regex_id, regexClass->varName) == 0)
            {
                aux = strdup(aux2->regex);
                break;
            }
            aux2 = aux2->next;
        }
        if (regexClass->closure == NULL /*dice estar de mas:  || regexClass->closure->closure == NULL*/)
        {
            return aux;
        }
        else
        {
            aux = _strConcat(aux, computeClosure(regexClass->closure));
            return aux;
        }
    }
}

void computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, automaton *automaton, return_struct *returner, uint64_t currentIndex)
{
    if (lexeme_precursor == NULL)
    {
        perror("You shouldn't be here mate");
        return;
    }
    switch (lexeme_precursor->precursor_type)
    {
    case literals:
        if (lexeme_precursor->type == default_lexeme)
        {
            if (has_default)
            {
                perror("There can only be one default lexeme");
                return;
            }
            else
            {
                has_default = true;
                uint64_t defaultStateIndex = returner == NULL ? 0 : new_state(automaton, 1, TEMP_TOKEN);
                for (unsigned char c = 0; c <= 127; c++)
                {
                    set_transition(automaton, 0, defaultStateIndex, c);
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
                    nextStateIndex = new_state(automaton, 1, TEMP_TOKEN);
                }
                set_transition(automaton, currentStateIndex, nextStateIndex, *s);
            }
            else
            {
                nextStateIndex = new_state(automaton, 0, 0);
                set_transition(automaton, currentStateIndex, nextStateIndex, *s);
            }
            s++;
            currentStateIndex = nextStateIndex;
        }
        return;
    case nonliterals:
        if (lexeme_precursor->lex_prec == NULL)
        {
            computeLexeme(lexeme_precursor->lex, automaton, currentIndex, returner, 1);
        }
        else
        {
            uint64_t finalState = computeLexeme(lexeme_precursor->lex, automaton, currentIndex, NULL, 0);
            computeLexemePrecursor(lexeme_precursor->lex_prec, automaton, returner, finalState);
        }
        return;
    }
}

uint64_t computeLexeme(Lexeme *lexeme, automaton *automaton, uint64_t currentIndex, return_struct *returner, boolean isEndOfChain)
{
    uint64_t finalState = currentIndex;
    Regexes *node;
    switch (lexeme->type)
    {
    case regexes:
        node = lexeme->regexes;
        break;
    case name: // al pedo hacer esto
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
        regexContent(node);
        return;
    }
    else
    {
        if (lexeme->closure->closure == PLUS)
        {
            finalState = regexContent(node);
        }
        finalState = regexContent(node);
    }

    return finalState;
}

return_struct *computeAction(Action *my_action)
{
    return_struct *returner = calloc(1, sizeof(return_struct));
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
}
