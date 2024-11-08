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

/** PRIVATE FUNCTIONS */
static void _addToList(Lexeme_precursor *lexeme, Action *returner);
static void _freeTransformerList(struct transformer_list *list);
static char *_strConcat(char *str1, char *str2);
void _ruleset(Ruleset *my_ruleset);
void _computeRule(Rule *my_rule);
void _regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex);
void _computeRegexClass(Regex_class *regexClass, uint64_t startIndex, uint64_t endIndex);
void _computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, Action *returner, uint64_t currentIndex);
uint64_t _computeLexeme(Lexeme *lexeme, uint64_t currentIndex, Action *returner, boolean isEndOfChain);

static void _addToList(Lexeme_precursor *lexeme, Action *returner)
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
}

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
}

void shutdownWeirdFlexModule()
{
    if (_logger != NULL)
    {
        destroyLogger(_logger);
    }
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
    Action *returner;
    switch (my_rule->type)
    {
    case lexeme_action:
        lexeme = my_rule->lex;
        returner = my_rule->action;
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

void _computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, Action *returner, uint64_t currentIndex)
{
    if (lexeme_precursor == NULL)
    {
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
                uint64_t defaultStateIndex = new_state(automat, 1, returner);
                for (unsigned char c = 9; c < 127; c++)
                {
                    if (!(c == 11 || c == 12 || (c >= 14 && c <= 31)))
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
                nextStateIndex = new_state(automat, 1, returner);
                set_transition(automat, currentStateIndex, nextStateIndex, *s);
            }
            else
            {
                if (*s == '\\')
                {
                    s++;
                    switch (*s)
                    {
                    case 'a':
                        *s = '\a';
                        break;

                    case 'b':
                        *s = '\b';
                        break;

                    case 't':
                        *s = '\t';
                        break;

                    case 'n':
                        *s = '\n';
                        break;

                    case 'v':
                        *s = '\v';
                        break;

                    case 'f':
                        *s = '\f';
                        break;

                    case 'r':
                        *s = '\r';
                        break;

                    case '\0':
                        result->succeed = false;
                        result->errorMessage = strdup("There can't be more than one default lexeme");
                        break;
                    default:
                        break;
                    }
                    if (!s[1])
                    {
                        nextStateIndex = new_state(automat, 1, returner);
                        set_transition(automat, currentStateIndex, nextStateIndex, *s);
                    }
                    else
                    {
                        nextStateIndex = new_state(automat, 0, NULL);
                        set_transition(automat, currentStateIndex, nextStateIndex, *s);
                    }
                }
                else
                {
                    nextStateIndex = new_state(automat, 0, NULL);
                    set_transition(automat, currentStateIndex, nextStateIndex, *s);
                }
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

uint64_t _computeLexeme(Lexeme *lexeme, uint64_t currentIndex, Action *returner, boolean isEndOfChain)
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
        if (isEndOfChain)
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
            if (isEndOfChain)
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
            if (isEndOfChain)
            {
                set_token(automat, currentIndex, returner);
            }
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
        return;
    }
}
