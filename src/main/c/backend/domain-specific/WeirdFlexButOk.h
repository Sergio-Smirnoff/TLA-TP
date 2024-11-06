#ifndef WEIRD_FLEX_HEADER
#define WEIRD_FLEX_HEADER

/**
 * We reuse the types from the AST for convenience, but you should separate
 * the layers of the backend and frontend using another group of
 * domain-specific models or DTOs (Data Transfer Objects).
 */
#include "../../frontend/syntactic-analysis/AbstractSyntaxTree.h"
#include "../../frontend/syntactic-analysis/BisonParser.h"
#include "../../shared/CompilerState.h"
#include "../../shared/Logger.h"
#include "../../shared/Type.h"
#include <limits.h>
#include <errno.h>

#include "../automaton/automaton.h"

/** Initialize module's internal state. */
void initializeWeirdFlexModule();

/** Shutdown module's internal state. */
void shutdownWeirdFlexModule();

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
            Token parameters;
            Block* java_block;
        };
    };
    return_type type;
} return_struct;

typedef struct transformer_list {
    Lexeme_precursor* lexeme;
    return_struct* returner;
    struct transformer_list* next;
}transformer_list;

/**
 * The result of a computation. It's considered valid only if "succeed" is
 * true.
 */
typedef struct {
	boolean succeed;
	transformer_list* value;
} ComputationResult;

// Checks the tree and builds lexemes table
ComputationResult* computeProgram(Program * tree, Valid_Regex_List* regexList);
void print_transformerlist(transformer_list* list);

#endif
