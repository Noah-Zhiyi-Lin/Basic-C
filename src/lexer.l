%option noyywrap
%option nounput
%option noinput
%top{
    #include <stdarg.h>
    //#include "common.h"
    //#include "parser.h"
}
%{
    extern dump_type_t dump_type;

    inline void dump_tokens(const char* format, ...) {
        va_list args;
        va_start(args, format);
        if (dump_type == TOKENS)
            vfprintf(yyout, format, args);
        va_end(args);
    }

    /* TODO: 初始化词法分析器的其他全局变量或状态信息 */
    int yylineno = 1;  // 行号初始化
%}

/* definitions section */
DECIMIAL ([1-9][0-9]*|0)
ID [[:alpha:]_][[:alpha:][:digit:]_]*
EOL (\r\n|\n|\r)
WHITE [\t ]
/* TODO: 定义其他正则表达式或宏，用于匹配更多的词法单元 */
OR "||"
NOT "!"
NOTEQUAL "!="
GREATER ">"
GREATEREQUAL ">="
LESSEQUAL "<="
MULTIPLY "\*"
DIVIDE "/"
MODULO "%"
SUBTRACT "-"
COMMA ","
DOT "."
PLUSPLUS "\+\+"
MINUSMINUS "--"
COMMENT("//.*")

%%


"int" {
    dump_tokens("INT\t%s\n", yytext);
    return INT;
}

"if" {
    dump_tokens("IF\t%s\n", yytext);
    return IF;
}

"else" {
    dump_tokens("ELSE\t%s\n", yytext);
    return ELSE;
}

"return" {
    dump_tokens("RETURN\t%s\n", yytext);
    return RETURN;
}

"&&" {
    dump_tokens("AND\t%s\n", yytext);
    return AND;
}

"=" {
    dump_tokens("ASSIGN\t%s\n", yytext);
    return ASSIGN;
}

"<" {
    dump_tokens("LESS\t%s\n", yytext);
    return LESS;
}

"+" {
    dump_tokens("ADD\t%s\n", yytext);
    return ADD;
}

";" {
    dump_tokens("SEMICOLON\t%s\n", yytext);
    return SEMICOLON;
}

"(" {
    dump_tokens("LPAREN\t%s\n", yytext);
    return LPAREN;
}

")" {
    dump_tokens("RPAREN\t%s\n", yytext);
    return RPAREN;
}

"{" {
    dump_tokens("LBRACE\t%s\n", yytext);
    return LBRACE;
}

"}" {
    dump_tokens("RBRACE\t%s\n", yytext);
    return RBRACE;
}

{DECIMIAL} {
    dump_tokens("NUMBER\t%s\n", yytext);
    return NUMBER;
}

{ID} {
    dump_tokens("ID\t%s\n", yytext);
    return ID;
}

{EOL} yylineno++;

{WHITE}

"||" {
    dump_tokens("OR\t%s\n", yytext);
    return OR;
}

"!" {
    dump_tokens("NOT\t%s\n", yytext);
    return NOT;
}

"!=" {
    dump_tokens("NOTEQUAL\t%s\n", yytext);
    return NOTEQUAL;
}

">" {
    dump_tokens("GREATER\t%s\n", yytext);
    return GREATER;
}

">=" {
    dump_tokens("GREATEREQUAL\t%s\n", yytext);
    return GREATEREQUAL;
}

"<=" {
    dump_tokens("LESSEQUAL\t%s\n", yytext);
    return LESSEQUAL;
}

"*" {
    dump_tokens("MULTIPLY\t%s\n", yytext);
    return MULTIPLY;
}

"/" {
    dump_tokens("DIVIDE\t%s\n", yytext);
    return DIVIDE;
}

"%" {
    dump_tokens("MODULO\t%s\n", yytext);
    return MODULO;
}

"-" {
    dump_tokens("SUBTRACT\t%s\n", yytext);
    return SUBTRACT;
}

"," {
    dump_tokens("COMMA\t%s\n", yytext);
    return COMMA;
}

"." {
    dump_tokens("DOT\t%s\n", yytext);
    return DOT;
}

"++" {
    dump_tokens("PLUSPLUS\t%s\n", yytext);
    return PLUSPLUS;
}

"--" {
    dump_tokens("MINUSMINUS\t%s\n", yytext);
    return MINUSMINUS;
}

{COMMENT} {
    // 处理注释，忽略不输出
}

. {
    yyerror("Unknown character: %s", yytext);
}

%%

/* 辅助函数 */
int yywrap() {
    return 1;  // 表示没有更多的输入文件
}
