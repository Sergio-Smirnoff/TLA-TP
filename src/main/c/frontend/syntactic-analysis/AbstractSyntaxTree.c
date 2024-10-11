#include "AbstractSyntaxTree.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;

void initializeAbstractSyntaxTreeModule() {
	_logger = createLogger("AbstractSyntxTree");
}

void shutdownAbstractSyntaxTreeModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
}

/** PUBLIC FUNCTIONS */


void releaseProgram(Program * program) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (program != NULL) {
		releaseRuleset(program->ruleset);
		free(program);
	}
}

void releaseRuleset(Ruleset * ruleset) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (ruleset != NULL) {
		releaseRule(ruleset->rule);
		releaseRuleset(ruleset->ruleset);
		free(ruleset);
	}
}

void releaseRule(Rule * rule) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (rule != NULL) {
		switch (rule->type) {
			case Rule_type.regex:
				releaseRegexClass(rule->regex_class);
				break;
			case Rule_type.ignore_lexeme:
				releaseLexeme(rule->lexeme);
				break;
			case Rule_type.lexeme_action:
				releaseLexeme(rule->lexeme);
				releaseAction(rule->action);
				break;
		}
		free(rule);
	}
}

void releaseAction(Action * action) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (action != NULL) {
		if ( action->type == Action_type.param ) {
			releaseParam(action->param);
		}
		free(action);
	}
}

void releaseParam(Param * param) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (param != NULL) {
		free(param);
	}
}

void releaseLexeme(Lexeme * lexeme) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (lexeme != NULL) {
		switch (lexeme->type) {
			case Lexeme_type.regex_class:
				releaseRegexClass(lexeme->regex_class);
				releaseClosure(lexeme->closure);
				break;
			case Lexeme_type.reg:
				releaseClosure(lexeme->closure);
				break;
		}
		free(lexeme);
	}
}

void releaseRegexClass(Regex_class * regex_class) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (regex_class != NULL) {
		if (regex_class->type == Regex_class_type.range) {
			releaseRange(regex_class->range);	
		}
		releaseRegexClass(regex_class->regex_class);
		free(regex_class);
	}
}

void releaseRange(Range * range) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (range != NULL) {
		free(range);
	}
}

void releaseClosure(Closure * closure) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (closure != NULL) {
		free(closure);
	}
}

void releaseFunctionBody(FunctionBody * functionBody) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (functionBody != NULL) {
		free(functionBody);
	}
}