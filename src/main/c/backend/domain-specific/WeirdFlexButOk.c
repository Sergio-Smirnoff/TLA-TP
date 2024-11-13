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

/** PRIVATE FUNCTIONS */
static void _addToList(Lexeme_precursor *lexeme, Action *returner);
static char *_strConcat(char *str1, char *str2);
void _ruleset(Ruleset *my_ruleset);
void _computeRule(Rule *my_rule);
void _regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex);
void _computeRegexClass(Regex_class *regexClass, uint64_t startIndex, uint64_t endIndex);
uint64_t _computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, Action *returner, uint64_t currentIndex, boolean useToken);
uint64_t _computeLexeme(Lexeme *lexeme, uint64_t currentIndex, Action *returner, boolean isEndOfChain, boolean useToken);

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
            _computeLexemePrecursor(aux->lexeme, aux->returner, 0, 1);
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
    free_automaton(automat);
    computationResult->automaton = dfa;
    return;
}

uint64_t _computeLexemePrecursor(Lexeme_precursor *lexeme_precursor, Action *returner, uint64_t currentIndex, boolean useToken)
{
    if (lexeme_precursor == NULL)
    {
        return currentIndex;
    }
    switch (lexeme_precursor->precursor_type)
    {
    case default_t:
        uint64_t defaultStateIndex = useToken ? new_state(automat, 1, returner) : new_state(automat, 0, NULL);
        for (unsigned char c = 9; c < 127; c++)
        {
            if (!(c == 11 || c == 12 || (c >= 14 && c <= 31)))
            {
                set_transition(automat, currentIndex, defaultStateIndex, c);
            }
        }
        return defaultStateIndex;
    case nonliterals:
        if (lexeme_precursor->lex_prec == NULL)
        {
            return _computeLexeme(lexeme_precursor->lex, currentIndex, returner, 1, useToken);
        }
        else
        {
            if (lexeme_precursor->chain_type == concatenation)
            {
                uint64_t finalState = _computeLexeme(lexeme_precursor->lex, currentIndex, NULL, 0, 0);
                return _computeLexemePrecursor(lexeme_precursor->lex_prec, returner, finalState, useToken);
            }
            else if(lexeme_precursor->chain_type == summation)
            {
                uint64_t finalState = new_state(automat, 0, NULL);
                set_transition(automat, _computeLexeme(lexeme_precursor->lex, currentIndex, returner, 0, useToken), finalState, LAMBDA);
                set_transition(automat, _computeLexemePrecursor(lexeme_precursor->lex_prec, returner, currentIndex, useToken), finalState, LAMBDA);
                return finalState;
            }
            return currentIndex;
        }
    }
}

char _mapEscapedChar(char c)
{
    switch (c)
    {
    case 'a':
        return '\a';
    case 'b':
        return '\b';
    case 't':
        return '\t';
    case 'n':
        return '\n';
    case 'v':
        return '\v';
    case 'f':
        return '\f';
    case 'r':
        return '\r';
    default:
        return 0;
    }
}

uint64_t _computeLexeme(Lexeme *lexeme, uint64_t currentIndex, Action *returner, boolean isEndOfChain, boolean useToken)
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
    case string_lexeme:
        char *s = lexeme->string;
        uint64_t currentStateIndex = currentIndex;
        uint64_t nextStateIndex;
        while (*s)
        {
            if (s[0] == '\\' && s[1])
            {
                s++;
                *s = _mapEscapedChar(*s);
            }
            nextStateIndex = s[1] || !useToken ? new_state(automat, 0, NULL) : new_state(automat, 1, returner);
            set_transition(automat, currentStateIndex, nextStateIndex, *s);
            set_transition(automat, currentStateIndex, nextStateIndex, *s);

            s++;
            currentStateIndex = nextStateIndex;
        }
        return currentStateIndex;
    case precursor_closure:
        if (lexeme->closure == NULL)
        {
            return _computeLexemePrecursor(lexeme->precursor, returner, currentIndex, useToken);
        }

        if (lexeme->closure->closure == PLUS)
        {
            currentIndex = _computeLexemePrecursor(lexeme->precursor, NULL, currentIndex, 0);
        }

        uint64_t aux = new_state(automat, 0, NULL);
        set_transition(automat, currentIndex, aux, LAMBDA);

        finalState = _computeLexemePrecursor(lexeme->precursor, NULL, aux, 0);
        uint64_t newFinal = useToken ? new_state(automat, 1, returner) : new_state(automat, 0, NULL);
        set_transition(automat, finalState, aux, LAMBDA);
        set_transition(automat, finalState, newFinal, LAMBDA);
        set_transition(automat, currentIndex, newFinal, LAMBDA);
        return newFinal;
    }
    if (lexeme->closure == NULL)
    {
        finalState = isEndOfChain && useToken ? new_state(automat, 1, returner) : new_state(automat, 0, NULL);
        _regexContent(node, currentIndex, finalState);
        return finalState;
    }

    if (lexeme->closure->closure == PLUS)
    {
        finalState = isEndOfChain && useToken ? new_state(automat, 1, returner) : new_state(automat, 0, NULL);
        _regexContent(node, currentIndex, finalState);
    }

    if (lexeme->closure->closure == STAR && isEndOfChain && useToken)
    {
        set_token(automat, currentIndex, returner);
    }

    _regexContent(node, finalState, finalState);

    return finalState;
}

void _regexContent(Regexes *regexes, uint64_t startIndex, uint64_t endIndex)
{
    _computeRegexClass(regexes->regexClass, startIndex, endIndex);
    if (regexes->regexes != NULL)
        _regexContent(regexes->regexes, startIndex, endIndex);
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
