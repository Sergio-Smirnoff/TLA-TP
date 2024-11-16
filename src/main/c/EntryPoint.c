#include "backend/code-generation/Generator.h"
#include "backend/domain-specific/WeirdFlexButOk.h"
#include "frontend/lexical-analysis/FlexActions.h"
#include "frontend/syntactic-analysis/AbstractSyntaxTree.h"
#include "frontend/syntactic-analysis/BisonActions.h"
#include "frontend/syntactic-analysis/SyntacticAnalyzer.h"
#include "shared/CompilerState.h"
#include "shared/Environment.h"
#include "shared/Logger.h"
#include "shared/String.h"

void freeRegexLists(CompilerState *compilerState);

/**
 * The main entry-point of the entire application. If you use "strtok" to
 * parse anything inside this project instead of using Flex and Bison, I will
 * find you, and I will kill you (Bryan Mills; "Taken", 2008).
 */
const int main(const int count, const char **arguments)
{
	Logger *logger = createLogger("EntryPoint");
	initializeFlexActionsModule();
	initializeBisonActionsModule();
	initializeSyntacticAnalyzerModule();
	initializeAbstractSyntaxTreeModule();
	initializeWeirdFlexModule();
	initializeGeneratorModule();

	// Logs the arguments of the application.
	for (int k = 0; k < count; ++k)
	{
		logDebugging(logger, "Argument %d: \"%s\"", k, arguments[k]);
	}

	CompilerState compilerState = {
		.abstractSyntaxTree = NULL,
		.succeed = false,
		.validRegexList = malloc(sizeof(Valid_Regex_List)),
		.invalidRegexList = malloc(sizeof(Invalid_Regex_List)),
		.automaton = NULL};

	if (compilerState.validRegexList != NULL)
	{
		compilerState.validRegexList->size = 0;
		compilerState.validRegexList->head = NULL;
	}
	if (compilerState.invalidRegexList != NULL)
	{
		compilerState.invalidRegexList->size = 0;
		compilerState.invalidRegexList->head = NULL;
	}

	const SyntacticAnalysisStatus syntacticAnalysisStatus = parse(&compilerState);
	CompilationStatus compilationStatus = SUCCEED;
	Program *program = compilerState.abstractSyntaxTree;
	if (syntacticAnalysisStatus == ACCEPT)
	{
		// ----------------------------------------------------------------------------------------
		// Beginning of the Backend... ------------------------------------------------------------
		logDebugging(logger, "Computing expression value...");

		ComputationResult *computationResult = computeProgram(program, compilerState.validRegexList);
		if (!computationResult->succeed)
		{
			logError(logger, "The computation phase rejects the input program.");
			logError(logger, "Error: %s", computationResult->errorMessage);
			free(computationResult->errorMessage);
			compilationStatus = FAILED;
		}
		else
		{
			buildAutomaton(computationResult);
			if (!computationResult->succeed)
			{
				logError(logger, "The computation phase rejects the input program.");
				logError(logger, "Error: %s", computationResult->errorMessage);
				free(computationResult->errorMessage);
				compilationStatus = FAILED;
			}
			else
			{
				compilerState.automaton = computationResult->automaton;
				generate(&compilerState);
			}
			free_automaton(computationResult->automaton);
			free(computationResult);
		}
		// ...end of the Backend. -----------------------------------------------------------------
		// ----------------------------------------------------------------------------------------
	}
	else
	{
		if (compilerState.invalidRegexList->size > 0)
		{
			Invalid_Regex_List_Node *current = compilerState.invalidRegexList->head;

			while (current != NULL)
			{
				logError(logger, "Invalid regex: %s\n", current->regex_id);
				current = current->next;
			}
		}

		logError(logger, "The syntactic-analysis phase rejects the input program.");

		compilationStatus = FAILED;
	}

	logDebugging(logger, "Releasing AST resources...");
	releaseProgram(program);
	freeRegexLists(&compilerState);

	logDebugging(logger, "Releasing modules resources...");
	shutdownGeneratorModule();
	shutdownWeirdFlexModule();
	shutdownAbstractSyntaxTreeModule();
	shutdownSyntacticAnalyzerModule();
	shutdownBisonActionsModule();
	shutdownFlexActionsModule();
	logDebugging(logger, "Compilation is done.");
	destroyLogger(logger);

	printf("Compilation %s.\n", compilationStatus == SUCCEED ? "succeeds" : "fails");
	return compilationStatus;
}

void freeRegexLists(CompilerState *compilerState)
{
	Invalid_Regex_List_Node *currentInvalid = compilerState->invalidRegexList->head;
	Invalid_Regex_List_Node *auxInvalid;

	while (currentInvalid != NULL)
	{
		auxInvalid = currentInvalid;
		currentInvalid = currentInvalid->next;
		free(auxInvalid);
	}

	Valid_Regex_List_Node *currentValid = compilerState->validRegexList->head;
	Valid_Regex_List_Node *auxValid;

	while (currentValid != NULL)
	{
		auxValid = currentValid;
		currentValid = currentValid->next;
		free(auxValid);
	}

	free(compilerState->invalidRegexList);
	free(compilerState->validRegexList);
}
