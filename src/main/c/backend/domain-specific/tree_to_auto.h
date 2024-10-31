#ifndef TREE_TO_AUTO_HEADER
#define TREE_TO_AUTO_HEADER

#include "../automaton/automaton.h"
#include "../../frontend/syntaxtic-analysis/AbstractSyntaxTree.h"
#include "../../shared/Type.h"

typedef enum return_type {
    RETURN_TOKEN,
    RETURN_STRING,
    JAVA_BLOCK
} return_type;


typedef struct return_struct {
    union{
        Token* token;
        char* string;
        struct{
            Token* parameters;
            Block* java_block;
        }
    };
    return_type type;
} return_struct;

typedef struct transformer_list {
    char* lexeme;
    return_struct returner;
    struct transformer_list* next;
}transformer_list;


// elaborates the tree
struct transformer_list* obtain_lexeme_table( Program * tree );
// frees the transformer list
int free_transformer_list(struct transformer_list* list);

#endif



