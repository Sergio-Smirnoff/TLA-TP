#include "WeirdFlexButOk.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;
static transformer_list* list;
static transformer_list* current;
static Valid_Regex_List* validRegexList;
static boolean has_default = false;
static FILE * logFile;

/** PRIVATE FUNCTIONS */
static void _addToList(char* lexeme, return_struct* returner);
static int _isInList(char* lexeme);
static void _freeTransformerList(struct transformer_list* list);
static char* _strConcat(char* str1, char* str2);
void ruleset(Ruleset* my_ruleset);
void computeRule(Rule* my_rule);
char* regexContent(Regexes* regexes);
char* computeRegexClass(Regex_class* regexClass);
char* computeClosure(Closure* closure);
char* computeLexemePrecursor(Lexeme_precursor* lexeme_precursor);
char* computeLexeme(Lexeme* lexeme);
return_struct* computeAction(Action* my_action);

static void _addToList(char* lexeme, return_struct* returner){
    if(lexeme != NULL && returner != NULL){
        current->next = (transformer_list*)calloc(1, sizeof(transformer_list));
        if (errno != 0){
            return; // podriamos loggear el error
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
        if (to_free->lexeme != NULL){
            free(to_free->lexeme);
        }
        if (to_free->returner != NULL){
            free(to_free->returner);
        }
        free(to_free);
    }
}

static char* _strConcat(char* str1, char* str2){
    char* aux = calloc((strlen(str1) + strlen(str2) + 1), sizeof(char));
    sprintf(aux, "%s%s", str1, str2);
    return aux;
}

/** PUBLIC FUNCTIONS */



void initializeWeirdFlexModule() {
	_logger = createLogger("Weird Flex");
    list = (struct transformer_list*)calloc(1, sizeof(struct transformer_list));
    current = list;
    logFile = fopen("Backend.log","a");
}

void shutdownWeirdFlexModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
    fclose(logFile);
    _freeTransformerList(list);
}

void print_transformerlist(transformer_list* list){
    transformer_list* aux = list;
    while (aux != NULL){
        if (aux->lexeme != NULL){
            printf("Lexeme: %s\n", aux->lexeme);
        }
        if (aux->returner != NULL){
            switch (aux->returner->type){
                case RETURN_STRING:
                    printf("Returner: %s\n", aux->returner->string);
                    break;
                case JAVA_BLOCK:
                    printf("Returner: %d\n", aux->returner->parameters);
                    printf("Returner: %p\n", aux->returner->java_block);
                    break;
                case RETURN_TOKEN:
                    printf("Returner: %ls\n", aux->returner->token);
                    break;
            }
        }
        aux = aux->next;
    }
}

ComputationResult* computeProgram(Program * tree, Valid_Regex_List* regexList) {
    ComputationResult* result = (ComputationResult*)calloc(1, sizeof(ComputationResult));
    validRegexList = regexList;

    ruleset(tree->ruleset);
    if ( list == NULL){
        result->succeed = false;
        return result;
    }
    result->succeed = true;
    result->value = list;
    return result;
}

void ruleset(Ruleset* my_ruleset){
    if (my_ruleset == NULL){
        return;
    }
    computeRule(my_ruleset->rule);
    ruleset(my_ruleset->ruleset);
}

void computeRule(Rule* my_rule){
    if (my_rule == NULL){
        return;
    }
    switch (my_rule->type){
        char* lexeme;
        return_struct* returner;
        case lexeme_action:
            lexeme = computeLexemePrecursor(my_rule->lex);
            fprintf(logFile, "Me llego un lexeme %s\n", lexeme);
            fflush(logFile);
            returner = computeAction(my_rule->action);
                        fprintf(logFile, "Did i compute?\n");
            fflush(logFile);
            _addToList(lexeme, returner);
                                    fprintf(logFile, "YES?\n");
            fflush(logFile);
            break;
        case ignore_lexeme:
            lexeme = computeLexemePrecursor(my_rule->lexeme);
            returner = NULL;
            _addToList(lexeme, returner);
            break;
        case regex: // para mi no hace falta hacer esto
            char *regex_content = regexContent(my_rule->regexes);
            fprintf(logFile, "Regex content: %s\n", regex_content);
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
        fprintf(logFile, "IM NULL\n");
        fflush(logFile);
        return computeRegexClass(regexes->regexClass);
    } else {
                fprintf(logFile, "IM CHAD NOT NULL\n");
        fflush(logFile);
        char* regex_class = computeRegexClass(regexes->regexClass); 
        char* regex_content = regexContent(regexes->regexes);
        char* aux = _strConcat(regex_class, regex_content);// problema con mallocs, posible solucion
        free(regex_class);  
        free(regex_content);
        return aux;
    }
}

char* computeRegexClass(Regex_class* regexClass) {
    if(regexClass == NULL) {
        fprintf(logFile, "IM WEAK\n");
        fflush(logFile);
        return "";
    }
    switch (regexClass->type) {
        case symbol:
            return regexClass->symbol->symbol_tok;
        case range:
            char* aux = malloc(4 * sizeof(char));
            aux[0] = regexClass->startSymbol->symbol_tok[0];
            aux[1] = '-';
            aux[2] = regexClass->endSymbol->symbol_tok[0];
            aux[3] = '\0';
            return aux;
        case variable:
            if(regexClass->closure == NULL || regexClass->closure->closure == NULL/* opción type: || regexClass->closure->type == NULL*/){
                return regexClass->varName;
            } else {
                char* aux = computeClosure(regexClass->closure); // string fijo
                return _strConcat(regexClass->varName, aux);
            }
    }
}

char* computeClosure(Closure* closure) {
    switch (closure->closure) {
        case 266:
            return "*";
        case 267:
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
    if ( lexeme_precursor == NULL )
        return "";
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
                            fprintf(logFile, "Not lit: l256 computeLexemePrecursor\n");
    fflush(logFile);
            char* lexeme_prec = computeLexemePrecursor(lexeme_precursor->lex_prec);
            char* lexeme = computeLexeme(lexeme_precursor->lex);
            char* aux = _strConcat(lexeme, lexeme_prec);
            if (strcmp(lexeme_prec, "") != 0){
                free(lexeme_prec);
            }
            free(lexeme);
            return aux;
    }
}

char* computeLexeme(Lexeme* lexeme) {
    char* aux = "";
    switch(lexeme->type)
    {
        case regexes:
            aux = regexContent(lexeme->regexes);
            break;
        case name: // al pedo hacer esto
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

    aux = _strConcat("[", aux);
    aux = _strConcat(aux, "]");
    if (lexeme->closure != NULL){
        char* closure = computeClosure(lexeme->closure);
        aux = _strConcat(aux, closure);
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
            if (my_action->param == NULL){
                returner->parameters = 0;
                returner->java_block = my_action->block;
            } else {
            returner->parameters = my_action->param->stuff;
            returner->java_block = my_action->block;
            }
    }       
    return returner;
}
