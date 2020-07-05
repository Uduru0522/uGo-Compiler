/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1

    int yydebug = 0;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    int nextAddr = 0;
    int tCount = 1;
    t_table globalTable = {
        .level = 0,
        .tableSize = 0,
        .head = NULL,
        .next = NULL
    };
    t_table *newest = &globalTable;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    void create_symbol();
    void insertSymbol(t_entry *e);
    void lookupSymbol(char *id, char* type);
    void dumpSymbol(t_table* t);
    void CreateNewTable();
    void DeleteNewestTable();
    void initEntry(t_entry *e, int index, char *name, char *type, int address , int lineNo, char *elementType);
    int findAddress(char* id);
    t_varType detectArthimeticTypeError(t_varType l, t_varType r, const char* operation);
    t_varType detectOperationUndefinedOnTypeError(t_varType operand, t_varType *allowed, size_t listSize, const char* operation);
    t_varType detectAssignTypeError(t_varType l, t_varType r, const char* assignOperation);
    t_varType findIdType(const char* id);
    int findId(const char* id);
    char* findArrayType(const char* id);
    t_varType parseTypeNameStringToEnum(const char* type_name);
    const char* parseEnumToTypeNameString(t_varType type);
    // Debugging functions.
    void printAllTable();
%}

%error-verbose
%debug

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    int b_val;
    char *type_name;
    char *operator;
    struct TypeInfo type_info;
    struct OperandInfo operand_info;
}

/* Token without return */
%token VAR //ID
//%token INT FLOAT BOOL STRING
%token NEWLINE
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LOR LAND
%token EQL NEQ LEQ GEQ
%token INC DEC
%token IF ELSE
%token FOR
%token PRINT PRINTLN

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <b_val> BOOL_LIT
%token <operand_info> ID 
%token <type_info> INT FLOAT STRING BOOL 


/* Nonterminal with return, which need to sepcify type */
//%type <type> Type TypeName ArrayType
%type <type_info> Type TypeName ArrayType
%type <operator> IncDecOp UnaryOp AssignOp
%type <operand_info> Literal Operand 
%type <operand_info> IndexExpr ConversionExpr
%type <operand_info> PrimaryExpr UnaryExpr Expression
%type <operand_info> ExprA ExprB ExprC ExprD ExprE ExprF

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */

%%
Program
    : StatementList         { DeleteNewestTable(); }
;

StatementList
    : StatementList Statement
    | Statement
;

Statement
	: DeclarationStmt NEWLINE
	| SimpleStmt NEWLINE
	| Block NEWLINE
	| IfStmt NEWLINE
	| ForStmt NEWLINE
	| PrintStmt NEWLINE
	| NEWLINE
;

DeclarationStmt
	: VAR ID Type                   { 
        t_entry *ptr = calloc(1, sizeof(t_entry));
        if(findId($2.info) == -1){
            char str[100] = "\0";
            strcat(str, $2.info);
            strcat(str, " redeclared in this block. previous declaration at line ");
            t_entry *e = newest->head;
            while(e != NULL){
                if(!strcmp(e->name, $2.info)){
                    char lineNumber[12];
                    sprintf(lineNumber, "%d", e->lineNo);
                    strcat(str, lineNumber);
                }
                e = e->next;
            }
            yyerror(str);
        }
        else{
            initEntry(ptr, newest->nextIndex++, $2.info, $3.type_name, nextAddr, yylineno, $3.element_type);
            insertSymbol(ptr);
        }
    }    
	| VAR ID Type '=' Expression    { 
        t_entry *ptr = calloc(1, sizeof(t_entry));
        if(findId($2.info) == -1){
            char str[100] = "\0";
            strcat(str, $2.info);
            strcat(str, " redeclared in this block. previous declaration at line ");
            t_entry *e = newest->head;
            while(e != NULL){
                if(!strcmp(e->name, $2.info)){
                    char lineNumber[12];
                    sprintf(lineNumber, "%d", e->lineNo);
                    strcat(str, lineNumber);
                }
                e = e->next;
            }
            yyerror(str);
        }
        else{
            initEntry(ptr, newest->nextIndex++, $2.info, $3.type_name, nextAddr, yylineno, $3.element_type);
            insertSymbol(ptr);
        }
    }    
;

Type
    : TypeName
    | ArrayType     
;

TypeName   
    : INT           { 
        $$.element_type = "-";
        $$.type_name = "int32"; 
    }
    | FLOAT         { 
        $$.element_type = "-";
        $$.type_name = "float32"; 
    }
    | STRING        { 
        $$.element_type = "-";
        $$.type_name = "string"; 
    }
    | BOOL          { 
        $$.element_type = "-";
        $$.type_name = "bool"; 
    }
;

ArrayType
    : '[' Expression ']' Type { 
        $$.type_name = "array";
        $$.element_type = $4.type_name; 
    }
;

Expression
    : ExprA
;

ExprA
    : ExprA LOR ExprB   { 
        t_varType allowed[] = {t_BOOL};
        detectOperationUndefinedOnTypeError($1.type, allowed, 2, "LOR");
        detectOperationUndefinedOnTypeError($3.type, allowed, 2, "LOR");
        $$.type = t_BOOL;
        printf("LOR\n"); 
    }
    | ExprB
;

ExprB
    : ExprB LAND ExprC  { 
        t_varType allowed[] = {t_BOOL};
        detectOperationUndefinedOnTypeError($1.type, allowed, 2, "LAND");
        detectOperationUndefinedOnTypeError($3.type, allowed, 2, "LAND");
        $$.type = t_BOOL;
        printf("LAND\n"); 
    }
    | ExprC
;

ExprC
    : ExprC '<' ExprD   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "LSS");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "LSS");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "LSS");
        }
        $$.type = t_BOOL;
        printf("LSS\n"); 
    }
    | ExprC '>' ExprD   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "GTR");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "GTR");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "GTR");
        }
        $$.type = t_BOOL;
        printf("GTR\n"); 
    }
    | ExprC LEQ ExprD   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "LEQ");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "LEQ");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "LEQ");
        }
        $$.type = t_BOOL;
        printf("LEQ\n"); 
    }
    | ExprC GEQ ExprD   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "GEQ");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "GEQ");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "GEQ");
        }
        $$.type = t_BOOL;
        printf("GEQ\n"); 
    }
    | ExprC EQL ExprD   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "EQL");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "EQL");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "EQL");
        }
        $$.type = t_BOOL;
        printf("EQL\n"); 
    }
    | ExprC NEQ ExprD   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "NEQ");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "NEQ");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "NEQ");
        }
        $$.type = t_BOOL;
        printf("NEQ\n"); 
    }
    | ExprD
;

ExprD
    : ExprD '+' ExprE   {
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "ADD");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "ADD");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "ADD");
        }
        printf("ADD\n");    
    }
    | ExprD '-' ExprE   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "SUB");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "SUB");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "SUB");
        }
        printf("SUB\n"); 
    }
    | ExprE
;

ExprE
    : ExprE '*' ExprF   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "MUL");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "MUL");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "MUL");
        }
        printf("MUL\n"); 
    }
    | ExprE '/' ExprF   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "QUO");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "QUO");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "QUO");
        }
        printf("QUO\n"); 
    }
    | ExprE '%' ExprF   { 
        t_varType allowed[] = {t_INT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 1, "REM");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 1, "REM");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "REM");
        }
        printf("REM\n"); 
    }
    | ExprF
;

ExprF
    : UnaryExpr
;

UnaryExpr
    : PrimaryExpr
    | UnaryOp UnaryExpr     { 
        printf("%s\n", $1); 
        $$.info = $2.info;
        $$.type = $2.type;
    }
;

UnaryOp
    : '+'   { $$ = "POS"; }
    | '-'   { $$ = "NEG"; }
    | '!'   { $$ = "NOT"; }
;

PrimaryExpr
    : Operand           
    | IndexExpr
    | ConversionExpr 
;

Operand
    : Literal               { 
        printf("%s\n", $1.info);
        $$.info = "Literal";
        $$.type = $1.type;
    }
    | ID                    { 
        // Reminder: returns -1 if id not in table.
        int temp = findAddress($1.info);
        if(temp == -1){
            char *str = calloc(1, sizeof(char) * (strlen("undefined: ") + strlen($1.info) + 1));
            strcat(str, "undefined: ");
            strcat(str, $1.info);
            ++yylineno;
            yyerror(str);
            --yylineno;
        }
        else{
            char addr[20];
            sprintf(addr, "%d", temp);
            
            char* part[5] = { "IDENT (name=", strdup($1.info), ", address=", addr, ")" };
            size_t sum = 0;
            for(int i = 0;i < 5;++i){
                sum += strlen(part[i]);
            }

            char* str = calloc(1, sizeof(char) * (++sum));
            str[0] = '\0';
            for(int i = 0;i < 5;++i){
                strcat(str, part[i]);
            }
            printf("%s\n", str);
            free(str);
            $$.info = strdup($1.info);
            $$.type = findIdType($1.info);
        }
    }
    |  '(' Expression ')'   { 
        $$.info = strdup($2.info);
        $$.type = $2.type;
    }
;

Literal
    : INT_LIT       { 
        char str[20];
        sprintf(str, "INT_LIT %d", $1);
        $$.info = strdup(str);
        $$.type = t_INT;
    }
    | FLOAT_LIT     { 
        char str[100];
        sprintf(str, "FLOAT_LIT %f", $1);
        $$.info = strdup(str);    
        $$.type = t_FLOAT;
    }
    | '"' STRING_LIT '"'   { 
        int len = strlen("STRING_LIT ") + strlen($2) + 2;
        char *str;
        str = calloc(len, sizeof(char));
        strcat(str, "STRING_LIT ");
        strcat(str, $2);
        $$.info = strdup(str);
        $$.type = t_STRING;
        free(str);
    }
    | BOOL_LIT      { 
        char *str;
        if($1){
            str = "TRUE";
        }
        else{
            str = "FALSE";
        }
        $$.info = strdup(str);
        $$.type = t_BOOL;
    }
;

IndexExpr
    : PrimaryExpr '[' Expression ']' { 
        $$.info = findArrayType($1.info); 
        $$.type = parseTypeNameStringToEnum($$.info);
    }
;

SimpleStmt
    : AssignmentStmt
    | ExpressionStmt
    | IncDecStmt
;

ConversionExpr
    : Type '(' Expression ')' { 
        $$.info = $1.type_name; 
        $$.type = parseTypeNameStringToEnum($1.type_name);
        char from, to;
        switch($3.type){
            case t_INT: from = 'I'; break;
            case t_FLOAT: from = 'F'; break;
            case t_STRING: from = 'S'; break;
            case t_ARRAY: from = 'A'; break;
            case t_BOOL: from = 'B'; break;
            default: from = 'E'; break;
        }
        switch(parseTypeNameStringToEnum($1.type_name)){
            case t_INT: to = 'I'; break;
            case t_FLOAT: to = 'F'; break;
            case t_STRING: to = 'S'; break;
            case t_ARRAY: to = 'A'; break;
            case t_BOOL: to = 'B'; break;
            default: to = 'E'; break;
        }
        printf("%c to %c\n", from, to);
    } 
;

AssignmentStmt
    : PrimaryExpr AssignOp Expression {
        if(!strcmp($1.info, "Literal")){
            char str[100] = "\0";
            strcat(str, "cannot assign to ");
            strcat(str, parseEnumToTypeNameString($1.type));
            yyerror(str);
        }
        else if($3.type != t_ERROR){
            detectAssignTypeError($1.type, $3.type, $2);
        }
        printf("%s\n", $2); 
    }
;

AssignOp
    : '='           { $$ = "ASSIGN"; }
    | ADD_ASSIGN    { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN    { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN    { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN    { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN    { $$ = "REM_ASSIGN"; }
;

ExpressionStmt
    : Expression
;

IncDecStmt
    : Expression IncDecOp 
;

IncDecOp
    : INC   { printf("INC\n"); }
    | DEC   { printf("DEC\n"); }
;

Block
    : BlockStart NEWLINE StatementList BlockEnd
    | BlockStart BlockEnd
;

BlockStart
    : '{'   { CreateNewTable(); }
;

BlockEnd
    : '}'   { DeleteNewestTable(); }
;

IfStmt
    : IF Condition Block Else
;

Else
    : 
    | ELSE IfStmt
    | ELSE Block

Condition
    : Expression {
        if($1.type != t_BOOL){
            char str[100] = "\0";
            strcat(str, "non-bool (type ");
            strcat(str, parseEnumToTypeNameString($1.type));
            strcat(str, ") used as for condition");
            ++yylineno;
            yyerror(str);
            --yylineno;
        }
    }
;

ForStmt
    : FOR Condition Block   {
        // Used to test the returned token textual position, which is weird.
        // printf("S=%d, E=%d\n", @1.first_line, @1.last_line);
        // printf("S=%d, E=%d\n", @2.first_line, @2.last_line);
        // printf("S=%d, E=%d\n", @3.first_line, @3.last_line);
    }
    | FOR ForClause Block
;

ForClause
    : InitStmt ';' Condition ';' PostStmt
;

InitStmt
    : SimpleStmt
;

PostStmt
    : SimpleStmt
;

PrintStmt
    : PRINT '(' Expression ')'      { 
        if($3.type == t_ARRAY){
            printf("PRINT %s\n", $3.info);
        }
        else{
            printf("PRINT %s\n", parseEnumToTypeNameString($3.type)); 
        }
    }
    | PRINTLN '(' Expression ')'    { 
        if($3.type == t_ARRAY){
            printf("PRINTLN %s\n", $3.info);
        }
        else{
            printf("PRINTLN %s\n", parseEnumToTypeNameString($3.type)); 
        }
    }
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yylineno = 0;
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

void create_symbol() {
    return;
}

void insertSymbol(t_entry *e) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", e->name, newest->level);
    if(newest->head == NULL){
        newest->head = e;
    }
    else{
        t_entry *ptr = newest->head;
        for(int i = 1;i < newest->tableSize;++i){
            ptr = ptr->next;
        }

        ptr->next = e;
    }
    ++newest->tableSize;
    ++nextAddr;

    return;
}

void lookupSymbol(char *id, char* type) {

    return;
}

void CreateNewTable(){
    // Get Newest table
    t_table *newTable = calloc(1, sizeof(t_table));

    // Initialize
    newTable->next = NULL;
    newTable->level = newest->level + 1;
    newTable->tableSize = 0;
    newTable->head = NULL;
    newest->next = newTable;
    newest = newTable;
    ++tCount;

    return;
}

void DeleteNewestTable(){

    // Delete all entry
    printf("> Dump symbol table (scope level: %d)\n", newest->level);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n", 
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    for(int i = 0;i < newest->tableSize;++i){
        dumpSymbol(newest);
        //--nextAddr;
        //printf("Success One!\n");
    }
    // Free memory of deleted table
    if(newest->level){
        free(newest);
    }
    --tCount;
    newest = &globalTable;
    for(int i = 1;i < tCount;++i){
        newest = newest->next;
    }
    
    return;
}

void dumpSymbol(t_table* t) {
    // Print target information
    printf("%-10d%-10s%-10s%-10d%-10d%s\n",
           t->head->index, t->head->name, t->head->type, t->head->address, t->head->lineNo, t->head->elementType);
    
    // Pop front and free
    t_entry *ptr = t->head->next;
    free(t->head);
    t->head = ptr;
    return;
}

void initEntry(t_entry *e, int index, char *name, char *type, int address , int lineNo, char *elementType){
    e->index = index;
    e->name = name;
    e->type = type;
    e->address = address;
    e->lineNo = lineNo;
    e->elementType = elementType;
    e->next = NULL;
    return;
}

int findAddress(char* id){
    // If id not found in newest table, find in 1 level less.
    for(int i = tCount - 1; i >= 0;--i){
        t_table *t = &globalTable;
        for(int j = 0;j < i;++j){
            t = t->next;
        }
        t_entry *e = t->head;
        for(int j = 0;j < t->tableSize;++j){
            if(!strcmp(e->name, id)){
                return e->address;
                break;
            }
            e = e->next;
        }
    }

    return -1;
}

t_varType detectArthimeticTypeError(t_varType l, t_varType r, const char* operation){
    if(l != r){
        char str[100] = "\0";
        strcat(str, "invalid operation: ");
        strcat(str, operation);
        strcat(str, " (mismatched types ");
        if(l == t_FLOAT){
            strcat(str, "float32 and int32)");
        }
        else{
            strcat(str, "int32 and float32)");
        }
        yyerror(str);
        return t_ERROR;
    }
    else{
        return l;
    }
}

t_varType detectOperationUndefinedOnTypeError(t_varType operand, t_varType *allowed, size_t listSize, const char* operation){
    for(size_t i = 0;i < listSize;++i){
        if(operand == allowed[i]){
            return operand;
        }
    }
    char str[100] = "\0";
    strcat(str, "invalid operation: (operator ");
    strcat(str, operation);
    strcat(str, " not defined on ");
    strcat(str, parseEnumToTypeNameString(operand));
    strcat(str, ")");
    yyerror(str);
    return t_ERROR;
}

t_varType detectAssignTypeError(t_varType l, t_varType r, const char* assignOperation){
    if(l != r){
        char str[100] = "\0";
        strcat(str, "invalid operation: ");
        strcat(str, assignOperation);
        strcat(str, " (mismatched types ");
        strcat(str, parseEnumToTypeNameString(l));
        strcat(str, " and ");
        strcat(str, parseEnumToTypeNameString(r));
        strcat(str, ")");
        yyerror(str);
        return t_ERROR;
    }
    return l;
}

t_varType findIdType(const char* id){
    // If id not found in newest table, find in 1 level less.
    for(int i = tCount - 1; i >= 0;--i){
        t_table *t = &globalTable;
        for(int j = 0;j < i;++j){
            t = t->next;
        }
        t_entry *e = t->head;
        for(int j = 0;j < t->tableSize;++j){
            if(!strcmp(e->name, id)){
                return parseTypeNameStringToEnum(e->type);
                break;
            }
            e = e->next;
        }
    }
    
    return t_NONE;
}

char* findArrayType(const char* id){
    // If id not found in newest table, find in 1 level less.
    for(int i = tCount - 1; i >= 0;--i){
        t_table *t = &globalTable;
        for(int j = 0;j < i;++j){
            t = t->next;
        }
        t_entry *e = t->head;
        for(int j = 0;j < t->tableSize;++j){
            if(!strcmp(e->name, id)){
                return e->elementType;
                break;
            }
            e = e->next;
        }
    }
    
    return "Error";
}

int findId(const char* id){
    t_entry *e = newest->head;
    for(int i = 0;i < newest->tableSize;++i){
        if(!strcmp(e->name, id)){
            return -1;
        }
        e = e->next;
    }
    // Not in table, safe
    return 0;
}

t_varType parseTypeNameStringToEnum(const char* type_name){
    if(!strcmp(type_name, "int32")){
        return t_INT;
    }
    else if(!strcmp(type_name, "float32")){
        return t_FLOAT;
    }
    else if(!strcmp(type_name, "bool")){
        return t_BOOL;
    }
    else if(!strcmp(type_name, "string")){
        return t_STRING;
    }
    else if(!strcmp(type_name, "array")){
        return t_ARRAY;
    }

    return t_NONE;
}

const char* parseEnumToTypeNameString(t_varType type){
    switch(type){
        case t_INT:
            return "int32";
            break;
        case t_FLOAT:
            return "float32";
            break;
        case t_BOOL:
            return "bool";
            break;
        case t_STRING:
            return "string";
            break;
        case t_ARRAY:
            return "array";
            break;
        default:
            return "Error.";
    }
}

void printAllTable(){
    t_table *t = &globalTable;
    printf("====================================================\n");
    while(t != NULL){
        if(newest == t){
            printf("|| The t_table *newest is representing this table.\n");
        }
        printf("|| Printing Table Level %d (Size=%d)\n", t->level, t->tableSize);
        t_entry *e = t->head;
        while(e != NULL){
            printf("|| \t\tEntry: Name=%s, Address=%d, ScopeLvl=%d\n", e->name, e->address, t->level);
            //sleep(1);
            e = e->next;
        }
        t = t->next;
    }
    printf("==============================(All Table printed)===\n");
    return;
}