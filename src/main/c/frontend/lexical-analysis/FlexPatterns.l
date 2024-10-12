%{
#include "FlexActions.h"
#include "LexicalAnalyzerContext.h"
%}

/**
 * Enable start condition manipulation functions.
 */
%option stack
%option debug
/**
 * Flex contexts (a.k.a. start conditions).
 *
 * @see https://westes.github.io/flex/manual/Start-Conditions.html
 */
%x MULTILINE_COMMENT
%x LINE_COMMENT
%x REGEX_CONTENT
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
<LINE_COMMENT>"\n"                              { EndLineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<LINE_COMMENT>.                                 { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

[Rr]"egex"                                      { BeginRegexLine(createLexicalAnalyzerContext()); }

[a-zA-Z0-9_$]+                                  { return VariableNameLexeme(createLexicalAnalyzerContext()); }

"["                                             { BEGIN(REGEX_CONTENT); BeginRegexContentLexeme(createLexicalAnalyzerContext()); }
<REGEX_CONTENT>[[:space:]]+                     { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<REGEX_CONTENT>[a-z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), LOWERCASE); }
<REGEX_CONTENT>[A-Z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), UPPERCASE); }
<REGEX_CONTENT>[0-9]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), DIGIT); }
<REGEX_CONTENT>"-"                              { return RegexContentLexeme(createLexicalAnalyzerContext(), RANGER); }
<REGEX_CONTENT>[^\-\[\]\\]                      { return RegexContentLexeme(createLexicalAnalyzerContext(), SYMBOL); }
<REGEX_CONTENT>"\\"[\^\-\[\]\\]                 { return RegexContentLexeme(createLexicalAnalyzerContext(), ESCAPED_SYMBOL); }
<REGEX_CONTENT>"]"                              { IgnoredLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"String"                                        { BEGIN(FUNCTION_BODY); return FunctionParamLexeme(createLexicalAnalyzerContext(), STRING_TYPE); }
"Integer"                                       { BEGIN(FUNCTION_BODY);  return FunctionParamLexeme(createLexicalAnalyzerContext(), INTEGER_TYPE); }
"Double"                                        { BEGIN(FUNCTION_BODY);  return FunctionParamLexeme(createLexicalAnalyzerContext(), DOUBLE_TYPE); }
"Boolean"                                       { BEGIN(FUNCTION_BODY); return FunctionParamLexeme(createLexicalAnalyzerContext(), BOOLEAN_TYPE); }
")"[[:space]]*"{"                               { BEGIN(FUNCTION_BODY);  BeginFunctionBodyLexeme(createLexicalAnalyzerContext()); }

<FUNCTION_BODY>[^{}]+                           { return FunctionBodyContentLexeme(createLexicalAnalyzerContext()); }
<FUNCTION_BODY>"}"                              { EndFunctionBodyLexeme(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"\""                                            { QuotesLexeme(createLexicalAnalyzerContext()); }

"}*"                                            { return ClauseOperator(createLexicalAnalyzerContext(), KLEENE); }
"}+"                                            { return ClauseOperator(createLexicalAnalyzerContext(), POSITIVE); }
"}"                                             { CloseBracesLexeme(createLexicalAnalyzerContext()); }
"{"                                             { OpenBracesLexeme(createLexicalAnalyzerContext()); }

"default"                                       { DefaultLexeme(createLexicalAnalyzerContext()); }

"->"                                            { ArrowLexeme(createLexicalAnalyzerContext()); }
"{"                                             { BEGIN(FUNCTION_BODY); BeginFunctionBodyLexeme(createLexicalAnalyzerContext()); }
"("                                             { BEGIN(FUNCTION_PARAM); BeginFunctionParamLexeme(createLexicalAnalyzerContext()); }


[[:space:]]+                                    { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
";"                                             { return EndLineLexeme(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
"\n"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
"\r"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
.                                               { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
%%

#include "FlexExport.h"

