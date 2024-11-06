#include "WeirdFlexButOk.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define TEMP_TOKEN 1000000

/* MODULE INTERNAL STATE */

static Logger *_logger = NULL;
// static uint64_t _stateCounter = 0;
static transformer_list *list;
static transformer_list *current = NULL;
static Valid_Regex_List *validRegexList;
// to make automaton a global variable
static automaton *automat;
static ComputationResult *result;
static boolean has_default = false;
// should be deleted
static FILE *logFile;

/** PRIVATE FUNCTIONS */
static void _addToList(Lexeme_precursor *lexeme, return_struct *returner);
static void _freeTransformerList(struct transformer_list *list);
static char *_strConcat(char *str1, char *str2);
void ruleset(Ruleset *my_ruleset);
void computeRule(Rule *my_rule);
void regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex);
void computeRegexClass(Regex_class *regexClass, uint64_t startIndex, uint64_t endIndex);
void computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, return_struct *returner, uint64_t currentIndex);
uint64_t computeLexeme(Lexeme *lexeme, uint64_t currentIndex, return_struct *returner, boolean isEndOfChain);
return_struct *computeAction(Action *my_action);

static void _addToList(Lexeme_precursor *lexeme, return_struct *returner)
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

void buildAutomaton(ComputationResult *computationResult)
{
    transformer_list *aux = computationResult->list;
    result = computationResult;
    automat = new_automaton();
    set_initial_state(automat, get_state(automat, new_state(automat, 0, 0)));
    // Probablemente debería ser algo así:
    // set_initial_state(automat, get_state(automat, new_state(automat, NULL, startState++)));
    // puede que con startState++ esté flasheando
    int i = 1;
    while (aux != NULL)
    {
        if (result->succeed == false)
        {
            return;
        }
        if (aux->lexeme != NULL)
        {
            computeLexemePrecursor(aux->lexeme, aux->returner, 0);
        }
        else
        {
            computationResult->succeed = false;
            computationResult->errorMessage = strdup("Lexeme is NULL. Bad built table.");
            return;
        }
        aux = aux->next;
        // if (aux->returner != NULL){
        //     switch (aux->returner->type){
        //         case RETURNING:
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
    }
    int fd = open("./automaton_out.Java", O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR);
    automaton *dfa = get_deterministic_equivalent(automat);
    free(automat);
    write_java_initialization(dfa, fd);
    computationResult->automaton = dfa;
    return;
}

ComputationResult *computeProgram(Program *tree, Valid_Regex_List *regexList)
{
    result = (ComputationResult *)calloc(1, sizeof(ComputationResult));
    validRegexList = regexList;

    ruleset(tree->ruleset);
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
        _addToList(lexeme, returner);
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

void regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex)
{
    if (regexes->regexes == NULL)
    {
        computeRegexClass(regexes->regexClass, startIndex, endIndex);
    }
    else
    {
        computeRegexClass(regexes->regexClass, startIndex, endIndex);
        regexContent(regexes->regexes, startIndex, endIndex);
        return;
    }
}

void computeRegexClass(Regex_class *regexClass, uint64_t startIndex, uint64_t endIndex)
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
    case variable: // @Patrick es por acá
        Valid_Regex_List_Node *aux2 = validRegexList->head;
        while (aux2 != NULL)
        {
            if (strcmp(aux2->regex_id, regexClass->varName) == 0)
            {
                regexContent(aux2->regex, startIndex, endIndex);
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

void computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, return_struct *returner, uint64_t currentIndex)
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
                fprintf(logFile, "There can only be one default lexeme\n");
                return;
            }
            else
            {
                has_default = true;
                uint64_t defaultStateIndex = returner == NULL ? 0 : new_state(automat, 1, TEMP_TOKEN);
                // Probablemente debería ser algo así: (Para esto el 2ndo param debería ser un puntero a return_struct o a void)
                // new_state(automaton, returner, TEMP_TOKEN);
                for (unsigned char c = 0; c <= 127; c++)
                {
                    set_transition(automat, 0, defaultStateIndex, c);
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
                    nextStateIndex = new_state(automat, 1, TEMP_TOKEN);
                    // Probablemente debería ser algo así: (Para esto el 2ndo param debería ser un puntero a return_struct o a void)
                    // nextStateIndex = new_state(automaton, returner, _stateCounter++);
                    // puede que con _stateCounter++ esté flasheando
                }
                set_transition(automat, currentStateIndex, nextStateIndex, *s);
            }
            else
            {
                nextStateIndex = new_state(automat, 0, 0);
                // Probablemente debería ser algo así: (Para esto el 2ndo param debería ser un puntero a return_struct o a void)
                // nextStateIndex = new_state(automaton, NULL, _stateCounter++);
                // puede que con _stateCounter++ esté flasheando
                set_transition(automat, currentStateIndex, nextStateIndex, *s);
            }
            s++;
            currentStateIndex = nextStateIndex;
        }
        return;
    case nonliterals:
        if (lexeme_precursor->lex_prec == NULL)
        {
            computeLexeme(lexeme_precursor->lex, currentIndex, returner, 1);
        }
        else
        {
            uint64_t finalState = computeLexeme(lexeme_precursor->lex, currentIndex, NULL, 0);
            computeLexemePrecursor(lexeme_precursor->lex_prec, returner, finalState);
        }
        return;
    }
}

uint64_t computeLexeme(Lexeme *lexeme, uint64_t currentIndex, return_struct *returner, boolean isEndOfChain)
{
    uint64_t initialState = currentIndex;
    uint64_t finalState;
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
        finalState = new_state(automat, 0, 0);
        // Probablemente debería ser algo así: (Para esto el 2ndo param debería ser un puntero a return_struct o a void)
        // finalState = new_state(automaton, isEndOfChain ? returner : NULL, _stateCounter++);
        regexContent(node, currentIndex, finalState);
    }
    else
    {
        if (lexeme->closure->closure == PLUS)
        {
            initialState = new_state(automat, 0, 0);
            // Probablemente debería ser algo así: (Para esto el 2ndo param debería ser un puntero a return_struct o a void)
            // initialState = new_state(automaton, NULL, _stateCounter++);
            regexContent(node, currentIndex, 0);
        }
        finalState = new_state(automat, 0, 0);
        // Probablemente debería ser algo así: (Para esto el 2ndo param debería ser un puntero a return_struct o a void)
        // finalState = new_state(automaton, isEndOfChain ? returner : NULL, _stateCounter++);
        regexContent(node, initialState, finalState);
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
