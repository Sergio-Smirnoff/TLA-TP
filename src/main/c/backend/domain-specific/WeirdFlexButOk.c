#include "WeirdFlexButOk.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;
static transformer_list* list;
static transformer_list* current;
static Valid_Regex_List* validRegexList;
static boolean has_default = false;

void initializeWeirdFlexModule() {
	_logger = createLogger("Weird Flex");
    list = (struct transformer_list*)calloc(1, sizeof(struct transformer_list));
    current = list;
}

void shutdownWeirdFlexModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
    _freeTransformerList(list);
}

/** PRIVATE FUNCTIONS */
static void _addToList(char* lexeme, return_struct* returner);
static int _isInList(char* lexeme);
static void _freeTransformerList(struct transformer_list* list);
static char* _strConcat(char* str1, char* str2);

static void _addToList(char* lexeme, return_struct* returner){
    while(lexeme != NULL && returner != NULL){
        current->next = (transformer_list*)calloc(1, sizeof(transformer_list));
        if (errno != 0){
            return NULL;
        }
        current = current->next;
    }

    current->lexeme = lexeme;
    current->returner = returner;
    current->next = NULL;
}

// check if it is in the return list
static int _isInList(char* lexeme){
    transformer_list* aux = list;
    while (aux != NULL){
        if (strcmp(aux->lexeme, lexeme) == 0){
            return 0;
        }
        aux = aux->next;
    }
    return 1;
}

static void _freeTransformerList(struct transformer_list* list){
    transformer_list* aux = list;
    while (aux != NULL){
        transformer_list* to_free = aux;
        aux = aux->next;
        free(to_free);
    }
}

static char* _strConcat(char* str1, char* str2){
    char* aux = calloc((strlen(str1) + strlen(str2)), sizeof(char));
    strcat(aux, str1);
    strcat(aux, str2);
    return aux;
}

/** PUBLIC FUNCTIONS */

struct transformer_list* computeRuleset(Program * tree, Valid_Regex_List* regexList) {
    validRegexList = regexList;
    ruleset(tree->ruleset);
    return list;
}

void ruleset(Ruleset* my_ruleset){
    rule(my_ruleset->rule);
    ruleset(my_ruleset->ruleset);
}

void rule(Rule* my_rule){
    switch (my_rule->type){
        case lexeme_action:
            char *lexeme = computeLexemePrecursor(my_rule->lex);
            return_struct* returner = computeAction(my_rule->action);
            add_to_list(lexeme, returner);
            break;
        case ignore_lexeme:
            char *lexeme = computeLexemePrecursor(my_rule->lexeme);
            return_struct* returner = NULL;
            add_to_list(lexeme, returner);
            break;
        case regex:
            char *regex_content = regexContent(my_rule->regexes);
            Valid_Regex_List_Node* aux = validRegexList->head;
            while (aux != NULL){
                if (strcmp(aux->regex_id, my_rule->our_regex_id) == 0){
                    aux->regex = regex_content;
                    return;
                }
                aux = aux->next;
            }
            break;
    }
}

char* regexContent(Regexes* regexes) {
    if(regexes->regexes == NULL) {
        return computeRegexClass(regexes->regexClass);
    } else {
        char* regex_class = computeRegexClass(regexes->regexClass);
        char* regex_content = regexContent(regexes->regexes);
        return _strConcat(regex_class, regex_content);
    }
}

char* computeRegexClass(Regex_class* regexClass) {
    switch (regexClass->type) {
        case symbol:
            return regexClass->symbol->symbol_tok;
        case range:
            char* aux = malloc(3 * sizeof(char));
            aux[0] = regexClass->startSymbol;
            aux[1] = '-';
            aux[2] = regexClass->endSymbol;
            return aux;
        case variable:
            if(regexClass->closure == NULL || regexClass->closure->closure == NULL/* opción type: || regexClass->closure->type == NULL*/){
                return regexClass->varName;
            } else {
                char* aux = computeClosure(regexClass->closure);
                return _strConcat(regexClass->varName, aux);
            }
    }
}

char* computeClosure(Closure* closure) {
    switch (closure->closure) {
        case STAR:
            return "*";
        case PLUS:
            return "+";
        default:
            return "";
    }
    /* esta opción va si le agregamos type a las clausuras, no se como usar el token
    switch (closure->type) {
        case star:
            return "*";
        case plus:
            return "+";
        default:
            return "";
    }
    */
}

char* computeLexemePrecursor(Lexeme_precursor* lexeme_precursor){
    // check if it is a string
    switch (lexeme_precursor->precursor_type){
        case literals:
            if(lexeme_precursor->type == default_lexeme) {
                if(has_default) {
                    perror("There can only be one default lexeme");
                    return NULL;
                } else {
                    has_default = true;
                    return "default";
                }
            }
            return lexeme_precursor->string;
        case nonliterals:
            return _strConcat(computeLexeme(lexeme_precursor->lex), computeLexemePrecursor(lexeme_precursor->lex_prec));
    }
}

char* computeLexeme(Lexeme* lexeme) {
    char* aux = "";
    switch(lexeme->type)
    {
        case regexes:
            aux = regexContent(lexeme->regexes);
            break;
        case name:
            Valid_Regex_List_Node* aux2 = validRegexList->head;
            while (aux2 != NULL){
                if (strcmp(aux2->regex_id, lexeme->our_regex_id) == 0){
                    aux = aux2->regex;
                    break;
                }
                aux2 = aux2->next;
            }
            break;
    }
    return aux;
}

return_struct* computeAction(Action* my_action){
    return_struct* returner = calloc(1, sizeof(return_struct));
    switch (my_action->type){
        case action:
            returner->type = RETURN_STRING;
            returner->string = my_action->varName;
            break;
        case function_body:
            returner->type = JAVA_BLOCK;
            returner->parameters = my_action->param->stuff;
            returner->java_block = my_action->block;
    }       
    return returner;
}
