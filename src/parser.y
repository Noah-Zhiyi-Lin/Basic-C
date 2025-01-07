%code top{
    #include <iostream>
    // #include <assert.h>
    #include "parser.h"
    #include "SymbolTable.h" // 导入符号表类
    // #include "Type.h" // 导入类型类
    // #include "Ast.h" // 导入抽象语法树的节点类
    // extern Ast ast; // 声明抽象语法树
    int yylex();
    int yyerror( char const * );
}

%code requires {
    //#include "Ast.h"
    //#include "SymbolTable.h"
    //#include "Type.h"
}

%union {
    int itype;
    char* strtype;
    char* exprtype;
    char* stmttype;
    char* type;
    //StmtNode* stmttype;
    //ExprNode* exprtype;
    //Type* type;
}

%start Program
// 定义终结符
%token <strtype> ID 
%token <itype> INTEGER
%token IF ELSE
%token INT VOID
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON
%token ADD SUB MULTIPLY DIVIDE MODULO
%token OR AND NOT LESS GREATER LESSEQUAL GREATEREQUAL NOTEQUAL  ASSIGN
%token COMMA DOT
%token PLUSPLUS MINUSMINUS
%token RETURN

// 定义非终结符
%nterm <stmttype> Stmts Stmt AssignStmt IfStmt ReturnStmt DeclStmt FuncStmt
%nterm <exprtype> Exp AddExp Cond LOrExp PrimaryExp LVal RelExp LAndExp
%nterm <type> Type

// 定义优先级
%precedence THEN
%precedence ELSE

// temp
%%
Program
    : Stmts                   { std::cout << "Program" << std::endl; }
    ;

Stmts
    : Stmt                    { std::cout << "|-Stmt" << std::endl; }
    | Stmts Stmt             { std::cout << "|-Stmts" << std::endl; }
    ;

Stmt
    : AssignStmt             { std::cout << "  |-AssignStmt" << std::endl; }
    | IfStmt                 { std::cout << "  |-IfStmt" << std::endl; }
    | ReturnStmt             { std::cout << "  |-ReturnStmt" << std::endl; }
    | DeclStmt               { std::cout << "  |-DeclStmt" << std::endl; }
    | FuncStmt               { std::cout << "  |-FuncStmt" << std::endl; }
    | SEMICOLON              { std::cout << "  |-Empty" << std::endl; }
    | LBRACE Stmts RBRACE    { std::cout << "  |-Block" << std::endl; }
    ;

DeclStmt
    : Type ID SEMICOLON      { std::cout << "    |-Decl: " << $2 << std::endl; }
    ;

Type
    : INT                    { std::cout << "      |-Type: int" << std::endl; }
    | VOID                   { std::cout << "      |-Type: void" << std::endl; }
    ;

AssignStmt
    : LVal ASSIGN Exp SEMICOLON  { std::cout << "    |-Assign" << std::endl; }
    ;

IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN
                            { std::cout << "    |-If" << std::endl; }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt
                            { std::cout << "    |-If-Else" << std::endl; }
    ;

ReturnStmt
    : RETURN Exp SEMICOLON   { std::cout << "    |-Return" << std::endl; }
    | RETURN SEMICOLON { std::cout << "    |-Return" << std::endl; }
    ;

Exp
    : AddExp                 { std::cout << "      |-Exp" << std::endl; }
    | LPAREN Exp RPAREN      { std::cout << "      |-Exp in Parentheses" << std::endl; $$ = $2; }
    ;

AddExp
    : PrimaryExp            
    | AddExp ADD PrimaryExp { std::cout << "        |-Add" << std::endl; }
    | AddExp SUB PrimaryExp { std::cout << "        |-Sub" << std::endl; }
    | AddExp MULTIPLY PrimaryExp { std::cout << "        |-Mul" << std::endl; }
    | AddExp DIVIDE PrimaryExp   { std::cout << "        |-Div" << std::endl; }
    | AddExp MODULO PrimaryExp  { std::cout << "        |-Mod" << std::endl; }
    ;

PrimaryExp
    : LVal                  
    | INTEGER              { std::cout << "          |-Number: " << $1 << std::endl; }   
    ;

LVal
    : ID                    { std::cout << "          |-ID: " << $1 << std::endl; }
    ;

Cond
    : LOrExp                { std::cout << "      |-Condition" << std::endl; }
    ;

LOrExp
    : LAndExp               
    | LOrExp OR LAndExp     { std::cout << "        |-Or" << std::endl; }
    ;

LAndExp
    : RelExp                
    | LAndExp AND RelExp    { std::cout << "        |-And" << std::endl; }
    ;

RelExp
    : Exp                   
    | RelExp LESS Exp       { std::cout << "        |-Less" << std::endl; }
    | RelExp GREATER Exp    { std::cout << "        |-Greater" << std::endl; }
    | RelExp LESSEQUAL Exp  { std::cout << "        |-LessEqual" << std::endl; }
    | RelExp GREATEREQUAL Exp { std::cout << "        |-GreaterEqual" << std::endl; }
    | RelExp NOTEQUAL Exp   { std::cout << "        |-NotEqual" << std::endl; }
    ;

FuncStmt
    : Type ID LPAREN RPAREN LBRACE Stmts RBRACE    
        {
            std::cout << "    |-Function: " << $2 << std::endl;
            std::cout << "      |-ReturnType: ";
            std::cout << "      |-Type: ";
            std::cout << "void" << std::endl;
            std::cout << "      |-Body" << std::endl;
        }
    ;
%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
