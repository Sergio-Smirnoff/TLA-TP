%{
#include "FlexActions.h"
#include "LexicalAnalyzerContext.h"
%}

/**
 * Enable start condition manipulation functions.
 */
%option stack

/**
 * Flex contexts (a.k.a. start conditions).
 *
 * @see https://westes.github.io/flex/manual/Start-Conditions.html
 */
%x MULTILINE_COMMENT
%x LINE_COMMENT
%x REGEX
%x REGEX_NAME
%x REGEX_PRECONTENT
%x REGEX_CONTENT
%x LEXEME
%x REGEX_LEXEME
%x OUR_REGEX_LEXEME
%x PRE_ACTION
%x ACTION
%x SIMPLE_ACTION
%x FUNCTION_PARAM
%x FUNCTION_BODY
/**
 * Reusable patterns.
 *
 * @see https://westes.github.io/flex/manual/Matching.html
 * @see https://westes.github.io/flex/manual/Patterns.html
 */

%%
"/*"								            { BEGIN(MULTILINE_COMMENT); BeginMultilineCommentLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>"*/"				            { EndMultilineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<MULTILINE_COMMENT>[[:space:]]+		            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>[^*]+			            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>.				            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"//"                                            { BEGIN(LINE_COMMENT); BeginLineCommentLexemeAction(createLexicalAnalyzerContext()); }
<LINE_COMMENT>"\n"                             { EndLineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<LINE_COMMENT>.                                { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"[Rr]egex"                                      { BEGIN(REGEX); BeginRegexLine(createLexicalAnalyzerContext()); }
<REGEX>[a-zA-Z]                                { BEGIN(REGEX_NAME); BeginRegexNameLexeme(createLexicalAnalyzerContext()); }

<REGEX_NAME>[[:space:]]+                       { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<REGEX_NAME>[a-zA-Z0-9_$]+                     { return RegexClassNameLexeme(createLexicalAnalyzerContext()); BEGIN(REGEX_PRECONTENT); }

<REGEX_PRECONTENT>"["                          { BEGIN(REGEX_CONTENT); BeginRegexContentLexeme(createLexicalAnalyzerContext()); }
<REGEX_PRECONTENT>[^[:space:]]                 { return UnknownLexemeAction(createLexicalAnalyzerContext()); }

<REGEX_CONTENT>[[:space:]]+                    { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<REGEX_CONTENT>[a-z]                           { return RegexContentLexeme(createLexicalAnalyzerContext(), LOWERCASE); }
<REGEX_CONTENT>[A-Z]                           { return RegexContentLexeme(createLexicalAnalyzerContext(), UPPERCASE); }
<REGEX_CONTENT>[0-9]                           { return RegexContentLexeme(createLexicalAnalyzerContext(), DIGIT); }
<REGEX_CONTENT>"-"                             { return RegexContentLexeme(createLexicalAnalyzerContext(), RANGER); }
<REGEX_CONTENT>[^\-\[\]\\]                     { return RegexContentLexeme(createLexicalAnalyzerContext(), SYMBOL); }
<REGEX_CONTENT>"\\"[\^\-\[\]\\]                { return RegexContentLexeme(createLexicalAnalyzerContext(), ESCAPED_SYMBOL); }
<REGEX_CONTENT>"]"                             { IgnoredLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"\""                                            { BEGIN(LEXEME); BeginStringLexeme(createLexicalAnalyzerContext()); }
<LEXEME>[^"]+                                  { return StringLexeme(createLexicalAnalyzerContext(), STR); }   
<LEXEME>"\""                                   { EndStringLexeme(createLexicalAnalyzerContext()); BEGIN(PRE_ACTION); }

"["                                             { BEGIN(REGEX_LEXEME); BeginClassLexeme(createLexicalAnalyzerContext()); }
<REGEX_LEXEME>[[:space:]]+                     { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<REGEX_LEXEME>[a-z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), MIN); }
<REGEX_LEXEME>[A-Z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), MAYUSC); }
<REGEX_LEXEME>[0-9]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), DIGIT); }
<REGEX_LEXEME>"-"                              { return RegexContentLexeme(createLexicalAnalyzerContext(), RANGER); }
<REGEX_LEXEME>[^\-\[\]\\]                      { return RegexContentLexeme(createLexicalAnalyzerContext(), SIMBOL); }
<REGEX_LEXEME>"\\"[\^\-\[\]\\]                 { return RegexContentLexeme(createLexicalAnalyzerContext(), ESCAPED_SIMBOL); }
<REGEX_LEXEME>"]"                              { BEGIN(PRE_ACTION); IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"{"                                             { BEGIN(OUR_REGEX_LEXEME); BeginOurClassLexeme(createLexicalAnalyzerContext()); }
<OUR_REGEX_LEXEME>[[:space:]]+                 { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<OUR_REGEX_LEXEME>[a-z]*                       { return RegexClassNameLexeme(createLexicalAnalyzerContext()); }
<OUR_REGEX_LEXEME>"}*"                         { return ClauseOperator(createLexicalAnalyzerContext(), KLEENE); BEGIN(PRE_ACTION); }
<OUR_REGEX_LEXEME>"}+"                         { return ClauseOperator(createLexicalAnalyzerContext(), POSITIVA); BEGIN(PRE_ACTION); }
<OUR_REGEX_LEXEME>"}"                          { IgnoredLexemeAction(createLexicalAnalyzerContext()); BEGIN(PRE_ACTION); }

"default"                                       { return DefaultLexeme(createLexicalAnalyzerContext()); }

<PRE_ACTION>"->"                               { IgnoredLexemeAction(createLexicalAnalyzerContext()); BEGIN(ACTION); }
<ACTION>[a-zA-Z_$]                             { BEGIN(SIMPLE_ACTION); BeginSimpleActionLexeme(createLexicalAnalyzerContext()); }
<ACTION>"{"                                    { BEGIN(FUNCTION_BODY); BeginFunctionBodyLexeme(createLexicalAnalyzerContext()); }
<ACTION>"("                                    { BEGIN(FUNCTION_PARAMS); BeginFunctionParamLexeme(createLexicalAnalyzerContext()); }

<SIMPLE_ACTION>[[:space:]]+                    { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<SIMPLE_ACTION>[a-zA-Z_$]*                     { return ActionNameLexeme(createLexicalAnalyzerContext()); }

<FUNCTION_PARAM>[[:space:]]+                   { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<FUNCTION_PARAM>"String"                       { return FunctionParamLexeme(createLexicalAnalyzerContext(), STRING_TYPE); BEGIN(AFTER_PARAM); }
<FUNCTION_PARAM>"Integer"                      { return FunctionParamLexeme(createLexicalAnalyzerContext(), INTEGER_TYPE); BEGIN(AFTER_PARAM); }
<FUNCTION_PARAM>"Double"                       { return FunctionParamLexeme(createLexicalAnalyzerContext(), DOUBLE_TYPE); BEGIN(AFTER_PARAM); }
<FUNCTION_PARAM>"Boolean"                      { return FunctionParamLexeme(createLexicalAnalyzerContext(), BOOLEAN_TYPE); BEGIN(AFTER_PARAM); }
<FUNCTION_PARAM>")"[[:space]]*"{"              { BEGIN(FUNCTION_BODY);  BeginFunctionBodyLexeme(createLexicalAnalyzerContext()); }

<FUNCTION_BODY>[^{}]+                          { return FunctionBodyContentLexeme(createLexicalAnalyzerContext()); }
<FUNCTION_BODY>"}"                             { EndFunctionBodyLexeme(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

[[:space:]]+                                    { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
";"                                             { EndLineLexeme(createLexicalAnalyserContext()); BEGIN(INITIAL); }
"\n"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
"\r"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
.                                               { return UnknownLexemeAction(createLexicalAnalyzerContext()); }

%%

#include "FlexExport.h"

