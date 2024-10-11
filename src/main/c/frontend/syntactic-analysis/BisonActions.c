#include "BisonActions.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;

void initializeBisonActionsModule() {
	_logger = createLogger("BisonActions");
}

void shutdownBisonActionsModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
}

/** IMPORTED FUNCTIONS */

extern unsigned int flexCurrentContext(void);

/* PRIVATE FUNCTIONS */

static void _logSyntacticAnalyzerAction(const char * functionName);

/**
 * Logs a syntactic-analyzer action in DEBUGGING level.
 */
static void _logSyntacticAnalyzerAction(const char * functionName) {
	logDebugging(_logger, "%s", functionName);
}

/* PUBLIC FUNCTIONS */

// new


Program * ProgramSemanticAction(CompilerState * compilerState, Ruleset * ruleset) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Program * program = calloc(1, sizeof(Program));
	program->ruleset = ruleset;
	compilerState->abstractSyntaxtTree = program;
	if (0 < flexCurrentContext()) {
		logError(_logger, "The final context is not the default (0): %d", flexCurrentContext());
		compilerState->succeed = false;
	}
	else {
		compilerState->succeed = true;
	}
	return program;
}

Ruleset* RulesetSemanticAction( Rule* rule, Ruleset* ruleset ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Ruleset * rt = calloc(1, sizeof(Ruleset));
	rt->rule = rule;
	rt->ruleset = ruleset;
	return rt;
}

Rule* RuleDefinitionSematicAction( Lexeme* lexeme, Action* action, Token* endline, Rule_type type ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Rule * rule = calloc(1, sizeof(Rule));
	rule->lexeme = lexeme;
	rule->action = action;
	rule->endline = endline;
	rule->type = type;
	return rule;
}

Rule* RuleNewRegexSemanticAction( char* our_regex_id, Regex_class* regex_class, Token* endline ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Rule * rule = calloc(1, sizeof(Rule));
	rule->our_regex_id = our_regex_id;
	rule->regex_class = regex_class;
	rule->endline = endline;
	rule->type = regex;
	return rule;
}

Lexeme* LexemeSemanticAction( char* string, Regex_class* regex_class, Closure* closure, Lexeme_type type ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Lexeme * lexeme = calloc(1, sizeof(Lexeme));
	lexeme->string = string;
	lexeme->regex_class = regex_class;
	lexeme->closure = closure;
	lexeme->type = type;
	return lexeme;
}

Closure* ClosureSemanticAction( char* string ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Closure * closure = calloc(1, sizeof(Closure));
	closure->closure = string;
	return closure;
}

Range* RangeSemanticAction( char* right, char* left ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Range * range = calloc(1, sizeof(Range));
	range->right = right;
	range->left = left;
	return range;
}

Param* ParamSemanticAction( Token* token ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Param * param = calloc(1, sizeof(Param));
	param->token = token;
	return param;
}

Action* ActionSemanticAction( char* string ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Action * action = calloc(1, sizeof(Action));
	action->action = string;
	action->type = Action_type.action;
	return action;
}

Action* ActionParamSemanticAction( Param* param, char* body ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Action * action = calloc(1, sizeof(Action));
	action->param = param;
	action->function_body = body;
	action->type = Action_type.function_body;
	return action;
}

Regex_class* RegexClassStringSemanticAction( char* string, Regex_class* regex_class ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Regex_class * regex_class = calloc(1, sizeof(Regex_class));
	regex_class->string = string;
	regex_class->regex_class = regex_class;
	regex_class->type = Regex_class_type.stuff
	return regex_class;
}

Regex_class* RegexClassRangeSemanticAction( Range* range, Regex_class* regex_class ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Regex_class * regex_class = calloc(1, sizeof(Regex_class));
	regex_class->range = range;
	regex_class->regex_class = regex_class;
	regex_class->type = Regex_class_type.range;
	return regex_class;
}

