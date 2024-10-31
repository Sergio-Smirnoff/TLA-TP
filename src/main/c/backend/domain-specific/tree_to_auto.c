
#include "tree_to_auto.h"


static transformer_list* list = (struct transformer_list*)calloc(sizeof(struct transformer_list));
static transformer_list* current = list;

add_to_list(char* lexeme, return_struct returner ){
    if ( lexeme != NULL && returner != NULL ){
        current->next = (transformer_list*)calloc(sizeof(transformer_list));
        if ( errno != 0 ){
            return NULL;
        }
        current = current->next;
    }

    current->lexeme = lexeme;
    current->return_struct = return_struct;
    
}

// check if it is in the return list
int is_in_list( char* lexeme ){
    transformer_list* aux = list;
    while ( aux != NULL ){
        if ( strcmp( aux->lexeme, lexeme ) == 0 ){
            return 0;
        }
        aux = aux->next;
    }
    return 1;
}

struct transformer_list* obtain_lexeme_table( Program * tree ){
    ruleset( tree->ruleset );
    return list;
}

void ruleset( Ruleset* ruleset ){
    rule( ruleset->rule );
    ruleset( ruleset->ruleset );
}

void rule( Rule* rule ){
    switch ( rule->type )
        case lexeme_action:
            char *lexeme = lexeme_precursor( rule->lexeme_precursor );
            return_struct returner = action( rule->action );
            add_to_list( lexeme, returner );
            break;
        case ignore_lexeme:
            char *lexeme = lexeme_precursor( rule->lexeme_precursor );
            return_struct returner = NULL;
            add_to_list( lexeme, returner );
            break;
}

char* lexeme_precursor( LexemePrecursor* lexeme_precursor ){
    // check if it is a string
    switch ( lexeme_precursor->type )
        case literal:
            return lexeme_precursor->string;
        case nonliteral:
            char* aux;
            strcat( aux, lexeme( lexeme_precursor->lexeme ) );
            aux = strcat( aux, lexeme_precursor( lexeme_precursor->lexeme_precursor ) );
            return aux;
}

return_struct action( Action* action ){
    switch ( action->type )
        case action:
            return_struct returner;
            returner.type = RETURN_STRING;
            returner.string = action->varName;
            return returner;
        case java_block:
            return_struct returner;
            returner.type = JAVA_BLOCK;
            returner.parameters = action->parameters;
            returner.java_block = action->java_block;
            return returner;
}


