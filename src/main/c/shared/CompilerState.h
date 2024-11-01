#ifndef COMPILER_STATE_HEADER
#define COMPILER_STATE_HEADER

#include "Type.h"

typedef struct Invalid_Regex_List Invalid_Regex_List;
typedef struct Invalid_Regex_List_Node Invalid_Regex_List_Node;
typedef struct Valid_Regex_List Valid_Regex_List;
typedef struct Valid_Regex_List_Node Valid_Regex_List_Node;

/**
 * The general status of a compilation.
*/
typedef enum {
	SUCCEED = 0,
	FAILED = 1
} CompilationStatus;

typedef struct Invalid_Regex_List_Node {
    struct Invalid_Regex_List_Node* next;
    char* regex;
} Invalid_Regex_List_Node;

typedef struct Invalid_Regex_List {
    unsigned long int size;
    struct Invalid_Regex_List_Node* head;
} Invalid_Regex_List;

typedef struct Valid_Regex_List_Node {
	struct Valid_Regex_List_Node* next;
	char* regex;
} Valid_Regex_List_Node;

typedef struct Valid_Regex_List {
    unsigned long int size;
    struct Valid_Regex_List_Node* head;
} Valid_Regex_List;

/**
 * The global state of the compiler. Should transport every data structure
 * needed across the different phases of a compilation.
 */
typedef struct {
	// The root node of the AST.
	void * abstractSyntaxTree;

	// A flag that indicates the current state of the compilation so far.
	boolean succeed;

    struct Valid_Regex_List *validRegexList;
    struct Invalid_Regex_List *invalidRegexList;
	// TODO: Add an stack to handle nested scopes.
	// TODO: Add a symbol table.
	// TODO: Add configuration.
	// TODO: ...

	// The computed value of the entire program (only for the calculator).
	int value;
} CompilerState;

#endif
