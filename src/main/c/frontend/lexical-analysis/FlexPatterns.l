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
%x STRING
%x FUNCTION_PARAM
/**
 * Reusable patterns.
 *
 * @see https://westes.github.io/flex/manual/Matching.html
 * @see https://westes.github.io/flex/manual/Patterns.html
 */

%%

"String"                                        {  Token token = FunctionParamLexeme(createLexicalAnalyzerContext(), STRING_TYPE); BEGIN(FUNCTION_BODY); return token; }
"Integer"                                       {  Token token = FunctionParamLexeme(createLexicalAnalyzerContext(), INTEGER_TYPE); BEGIN(FUNCTION_BODY); return token; }
"Double"                                        {  Token token = FunctionParamLexeme(createLexicalAnalyzerContext(), DOUBLE_TYPE); BEGIN(FUNCTION_BODY); return token; }
"Boolean"                                       {  Token token = FunctionParamLexeme(createLexicalAnalyzerContext(), BOOLEAN_TYPE); BEGIN(FUNCTION_BODY); return token; }
"default"                                       { return DefaultLexeme(createLexicalAnalyzerContext()); }
"\n"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
"\r"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"/*"								            { BEGIN(MULTILINE_COMMENT); BeginMultilineCommentLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>"*/"				            { EndMultilineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<MULTILINE_COMMENT>[[:space:]]+		            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>[^*]+			            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>.				            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"//"                                            { BEGIN(LINE_COMMENT); BeginLineCommentLexemeAction(createLexicalAnalyzerContext()); }
<LINE_COMMENT>"\n"                              { EndLineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<LINE_COMMENT>.                                 { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

[Rr]"egex"                                      { BeginRegexLine(createLexicalAnalyzerContext()); }

[a-zA-Z0-9_$]+                                  { return RegexClassNameLexeme(createLexicalAnalyzerContext()); }

"["                                             { BEGIN(REGEX_CONTENT); BeginRegexContentLexeme(createLexicalAnalyzerContext()); }
<REGEX_CONTENT>[[:space:]]+                     { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
<REGEX_CONTENT>[a-z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), LOWERCASE); }
<REGEX_CONTENT>[A-Z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), UPPERCASE); }
<REGEX_CONTENT>[0-9]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), DIGIT); }
<REGEX_CONTENT>"-"                              { return RegexContentLexeme(createLexicalAnalyzerContext(), RANGER); }
<REGEX_CONTENT>[^\-\[\]\\]                      { return RegexContentLexeme(createLexicalAnalyzerContext(), SYMBOL); }
<REGEX_CONTENT>"\\"[\^\-\[\]\\]                 { return RegexContentLexeme(createLexicalAnalyzerContext(), ESCAPED_SYMBOL); }
<REGEX_CONTENT>"]"                              { IgnoredLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"("                                             { Token token = OpenParenthesesLexeme(createLexicalAnalyzerContext()); return token; }
")"[[:space]]*"{"                               { BEGIN(FUNCTION_BODY);  BeginFunctionBodyLexeme(createLexicalAnalyzerContext()); }

<FUNCTION_BODY>[^{}]+                           { return FunctionBodyContentLexeme(createLexicalAnalyzerContext()); }
<FUNCTION_BODY>"}"                              { EndFunctionBodyLexeme(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"\""                                            { BeginStringLexeme(createLexicalAnalyzerContext()); BEGIN(STRING); }
<STRING>[^"]+                                   { return StringLexeme(createLexicalAnalyzerContext()); }   
<STRING>"\""                                    { EndStringLexeme(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"}*"                                            { return ClauseOperator(createLexicalAnalyzerContext(), KLEENE); }
"}+"                                            { return ClauseOperator(createLexicalAnalyzerContext(), POSITIVE); }
"}"                                             { Token token = CloseBracesLexeme(createLexicalAnalyzerContext()); return token; }
"{"                                             { Token token = OpenBracesLexeme(createLexicalAnalyzerContext()); return token; }



"-> "                                            { return ArrowLexeme(createLexicalAnalyzerContext()); }

[[:space:]]+                                    { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
";"                                             { return EndLineLexeme(createLexicalAnalyzerContext()); }
.                                               { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
%%

#include "FlexExport.h"

