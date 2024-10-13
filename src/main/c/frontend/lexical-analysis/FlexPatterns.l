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
%x STRING
/**
 * Reusable patterns.
 *
 * @see https://westes.github.io/flex/manual/Matching.html
 * @see https://westes.github.io/flex/manual/Patterns.html
 */

%%
"return"                                        { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_RETURN); }
"if"                                            { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_IF); }
"else"                                          { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_ELSE); }
"for"                                           { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_FOR); }
"while"                                         { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_WHILE); }
"true"                                          { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_TRUE); }
"false"                                         { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_FALSE); }
"throw"                                         { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_THROW); }
"new"                                           { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_NEW); }
"?"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_TERNARY_OPERATOR); }
":"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_DOTS_OPERATOR); }
"<"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_LESSER); }
"<="                                            { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_LEQ); }
">"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_GREATER); }
">="                                            { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_GEQ); }
"=="|"!="                                            { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_EXACT_COMPARISON); }
"="|"*="|"/="|"%="|"+="|"-="|"<<="|">>="|">>>="|"&="|"^="|"|="    { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_ASSIGNMENT); }
"||"                                            { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_OR); }
"&&"                                            { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_AND); }
"!"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_NOT); }
"."                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), JAVA_DOT_OPERATOR); }

"String"                                        {  return FunctionParamLexeme(createLexicalAnalyzerContext(), STRING_TYPE);  }
"Integer"                                       {  return FunctionParamLexeme(createLexicalAnalyzerContext(), INTEGER_TYPE); }
"Double"                                        {  return FunctionParamLexeme(createLexicalAnalyzerContext(), DOUBLE_TYPE);  }
"Boolean"                                       {  return FunctionParamLexeme(createLexicalAnalyzerContext(), BOOLEAN_TYPE); }

"default"                                       { return DefaultLexeme(createLexicalAnalyzerContext()); }
"\n"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
"\r"                                            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

[Rr]"egex"                                      { BeginRegexLine(createLexicalAnalyzerContext()); }



"/*"								            { BEGIN(MULTILINE_COMMENT); BeginMultilineCommentLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>"*/"				            { EndMultilineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<MULTILINE_COMMENT>[[:space:]]+		            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>[^*]+			            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>.				            { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"//"                                            { BEGIN(LINE_COMMENT); BeginLineCommentLexemeAction(createLexicalAnalyzerContext()); }
<LINE_COMMENT>"\n"                              { EndLineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<LINE_COMMENT>.                                 { IgnoredLexemeAction(createLexicalAnalyzerContext()); }

"\""                                            { BeginStringLexeme(createLexicalAnalyzerContext()); BEGIN(STRING); }
<STRING>[^"]+                                   { return StringLexeme(createLexicalAnalyzerContext()); }   
<STRING>"\""                                    { EndStringLexeme(createLexicalAnalyzerContext()); BEGIN(INITIAL); }

"*"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), STAR); }
"++"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), INCREMENT); }
"--"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), DECREMENT); }
"+"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), PLUS); }
"-"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), MINUS); }
"/"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), DIV); }
"%"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), MOD); }
","                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), COMMA); }
"}"                                             { Token token = CloseBracesLexeme(createLexicalAnalyzerContext()); return token; }
"{"                                             { Token token = OpenBracesLexeme(createLexicalAnalyzerContext()); return token; }
"-> "                                           { return ArrowLexeme(createLexicalAnalyzerContext()); }
[[:space:]]+                                    { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
";"                                             { return EndLineLexeme(createLexicalAnalyzerContext()); }
"|"                                             { return SimpleTokenInsert(createLexicalAnalyzerContext(), PIPE); }


"("                                             { return OpenParenthesesLexeme(createLexicalAnalyzerContext()); }
")"                                             { return CloseParenthesesLexeme(createLexicalAnalyzerContext()); }

[a-zA-Z_$][a-zA-Z0-9_$]*                                  { return VarNameLexeme(createLexicalAnalyzerContext()); }
[0-9]+                                          { return SimpleTokenInsert(createLexicalAnalyzerContext(), NUMBER); }
[0-9]+"."[0-9]+                                 { return SimpleTokenInsert(createLexicalAnalyzerContext(), FLOAT); }

"["                                             { BEGIN(REGEX_CONTENT); return SimpleTokenInsert(createLexicalAnalyzerContext(), OPEN_BRACKET); }
<REGEX_CONTENT>[a-z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), LOWERCASE); }
<REGEX_CONTENT>[A-Z]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), UPPERCASE); }
<REGEX_CONTENT>[0-9]                            { return RegexContentLexeme(createLexicalAnalyzerContext(), DIGIT); }
<REGEX_CONTENT>"-"                              { return RegexContentLexeme(createLexicalAnalyzerContext(), RANGER); }
<REGEX_CONTENT>[^\-\[\]\\ ]                     { return RegexContentLexeme(createLexicalAnalyzerContext(), SYMBOL); }
<REGEX_CONTENT>"\\"[\^\-\[\]\\]                 { return RegexContentLexeme(createLexicalAnalyzerContext(), ESCAPED_SYMBOL); }
<REGEX_CONTENT>"]"                              { BEGIN(INITIAL); return SimpleTokenInsert(createLexicalAnalyzerContext(), CLOSE_BRACKET);}



.                                               { return UnknownLexemeAction(createLexicalAnalyzerContext()); }
%%

#include "FlexExport.h"

