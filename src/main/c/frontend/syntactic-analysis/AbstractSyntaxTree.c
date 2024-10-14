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

/*
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
			case regex:
				releaseRegexClass(rule->regex_class);
				break;
			case ignore_lexeme:
				releaseLexemePrecursor(rule->lexeme);
				break;
			case lexeme_action:
				releaseLexemePrecursor(rule->lexeme);
				releaseAction(rule->action);
				break;
		}
		free(rule);
	}
}

void releaseAction(Action * act) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (act != NULL) {
		if ( act->type == action ) {
			releaseParam(act->param);
		}
		free(act);
	}
}

void releaseParam(Param * param) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (param != NULL) {
		free(param);
	}
}

void releaseLexemePrecursor(Lexeme_precursor * lexeme_precursor) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (lexeme_precursor != NULL) {
		releaseLexeme(lexeme_precursor->lex);
		releaseLexemePrecursor(lexeme_precursor->lex_prec);
		free(lexeme_precursor);
	}
}

void releaseLexeme(Lexeme * lexeme) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (lexeme != NULL) {
		switch (lexeme->type) {
			case regex_class:
				releaseRegexClass(lexeme->regex_class);
				releaseClosure(lexeme->closure);
				break;
			case reg:
				releaseClosure(lexeme->closure);
				break;
		}
		free(lexeme);
	}
}

void releaseRegexClass(Regex_class * regex_class) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (regex_class != NULL) {
		if (regex_class->type == range) {
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

void releaseFunctionBody(Function_body * functionBody) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (functionBody != NULL) {
		free(functionBody);
	}
}
*/