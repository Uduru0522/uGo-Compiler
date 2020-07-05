/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    #define YYDEBUG 1

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

    // Output hw3.j
    FILE *jFile;
    char* jOutputBuffer;
    bool hasError = false;
    int compNumOfTime = 0;
    int boolPrintNumOfTime = 0;
    int ifBlockNumOfTime = 0;
    int wholeIfSectionCount = 0;
    int forSectionCount = 0;

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
    int findId(const char* id);;
    char* findArrayType(const char* id);
    t_varType parseTypeNameStringToEnum(const char* type_name);
    const char* parseEnumToTypeNameString(t_varType type);
    void loadNewVariable(t_entry *e);
    void printInstruction(t_printType pt, t_varType vt);
    void compareInstruction(t_varType type, char* jump_inst);
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
	: DeclarationStmt NEWLINE   { fprintf(jFile, NewLineBarrier); }
	| SimpleStmt NEWLINE        { fprintf(jFile, NewLineBarrier); }
	| Block NEWLINE             
	| IfStmt NEWLINE            { fprintf(jFile, NewLineBarrier); }
	| ForStmt NEWLINE           { fprintf(jFile, NewLineBarrier); }
	| PrintStmt NEWLINE         { fprintf(jFile, NewLineBarrier); }
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

            // HW3 Generation
            switch(parseTypeNameStringToEnum($3.type_name)){
                case t_INT: case t_BOOL:
                    fprintf(jFile, "\tldc 0\n");
                    break;
                case t_FLOAT:
                    fprintf(jFile, "\tldc 0.0\n");
                    break;
                case t_STRING:
                    fprintf(jFile, "\tldc \"\"\n");
                    break;
                default: break;
            }
            loadNewVariable(ptr);
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

            // HW3 Generation
            loadNewVariable(ptr);
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
        fprintf(jFile, "\tior\n");
    }
    | ExprB
;

ExprB
    : ExprB LAND ExprC  { 
        t_varType allowed[] = {t_BOOL};
        detectOperationUndefinedOnTypeError($1.type, allowed, 2, "LAND");
        detectOperationUndefinedOnTypeError($3.type, allowed, 2, "LAND");
        $$.type = t_BOOL;
        fprintf(jFile, "\tiand\n");
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

         // HW3 Generation
        compareInstruction($1.type, "iflt");
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

         // HW3 Generation
        compareInstruction($1.type, "ifgt");
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

         // HW3 Generation
        compareInstruction($1.type, "ifle");
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

         // HW3 Generation
        compareInstruction($1.type, "ifge");
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

         // HW3 Generation
        compareInstruction($1.type, "ifeq");
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

        // HW3 Generation
        compareInstruction($1.type, "ifne");
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

        // HW3 Generation
        switch($1.type){
            case t_INT:
                fprintf(jFile, "\tiadd\n");
                break;
            case t_FLOAT:
                fprintf(jFile, "\tfadd\n");
                break;
            default: break;
        }
    }
    | ExprD '-' ExprE   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "SUB");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "SUB");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "SUB");
        }
        
        // HW3 Generation
        switch($1.type){
            case t_INT:
                fprintf(jFile, "\tisub\n");
                break;
            case t_FLOAT:
                fprintf(jFile, "\tfsub\n");
                break;
            default: break;
        }
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

        // HW3 Generation
        switch($1.type){
            case t_INT:
                fprintf(jFile, "\timul\n");
                break;
            case t_FLOAT:
                fprintf(jFile, "\tfmul\n");
                break;
            default: break;
        }
    }
    | ExprE '/' ExprF   { 
        t_varType allowed[] = {t_INT, t_FLOAT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 2, "QUO");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 2, "QUO");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "QUO");
        }

        // HW3 Generation
        switch($1.type){
            case t_INT:
                fprintf(jFile, "\tidiv\n");
                break;
            case t_FLOAT:
                fprintf(jFile, "\tfdiv\n");
                break;
            default: break;
        }
    }
    | ExprE '%' ExprF   { 
        t_varType allowed[] = {t_INT};
        t_varType status, status2;
        status = detectOperationUndefinedOnTypeError($1.type, allowed, 1, "REM");
        status2 = detectOperationUndefinedOnTypeError($3.type, allowed, 1, "REM");
        if(status != t_ERROR && status2 != t_ERROR){
            detectArthimeticTypeError($1.type, $3.type, "REM");
        }

        // HW3 Generation
        switch($1.type){
            case t_INT:
                fprintf(jFile, "\tirem\n");
                break;
            case t_FLOAT:
                fprintf(jFile, "\tfrem\n");
                break;
            default: break;
        }
    }
    | ExprF
;

ExprF
    : UnaryExpr
;

UnaryExpr
    : PrimaryExpr           {
        if($1.type == t_ARRAY){
            switch($$.type = parseTypeNameStringToEnum($1.info)){
                case t_INT:
                    fprintf(jFile, "\tiaload\n");
                    break;
                case t_FLOAT:
                    fprintf(jFile, "\tfaload\n");
                    break;
                default: break;
            }
        }
        //fprintf(jFile, "====%s\t%d\n", $1.info, $1.type);
    }
    | UnaryOp UnaryExpr     {
        $$.info = $2.info;
        $$.type = $2.type;
        
        if(!strcmp($1, "NEG")){
            switch($2.type){
                case t_INT:
                    fprintf(jFile, "\tineg\n");
                    break;
                case t_FLOAT:
                    fprintf(jFile, "\tfneg\n");
                    break;
                default: break;
            }
        }
        else if(!strcmp($1, "NOT")){
            fprintf(jFile, "\ticonst_1\n"
                           "\tixor\n");
        }
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
            hasError = true;
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
            free(str);
            $$.info = strdup($1.info);
            $$.type = findIdType($1.info);
        
            switch($$.type){
                case t_INT: case t_BOOL:
                    fprintf(jFile, "\tiload %d\n", temp);
                    break;
                case t_FLOAT:
                    fprintf(jFile, "\tfload %d\n", temp);
                    break;
                case t_STRING:
                    fprintf(jFile, "\taload %d\n", temp);
                    break;
                default: break;
            }
        }
        // fprintf(jFile, "ID info:\n"
        //                "%s\t%d\n", $1.info, $1.type);
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

        // HW3 Generation
        fprintf(jFile, "\tldc %d\n", $1);
    }
    | FLOAT_LIT     { 
        char str[100];
        sprintf(str, "FLOAT_LIT %f", $1);
        $$.info = strdup(str);    
        $$.type = t_FLOAT;

        // HW3 Generation
        fprintf(jFile, "\tldc %f\n", $1);
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
        
        // HW3 Generation
        fprintf(jFile, "\tldc \"%s\"\n", $2);
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

        // HW3 Generation
        if($1){
            fprintf(jFile, "\ticonst_1\n");
        }
        else{
            fprintf(jFile, "\ticonst_0\n");
        }
    }
;

IndexExpr
    : PrimaryExpr '[' Expression ']' { 
        $$.info = findArrayType($1.info); 
        $$.type = t_ARRAY;
        fprintf(jFile, "\taload %d\n"
                       "\tswap\n", findAddress($1.info));
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

        if(from == 'F' && to == 'I'){
            fprintf(jFile, "\tf2i\n");
        }
        else if(from == 'I' && to == 'F'){
            fprintf(jFile, "\ti2f\n");
        }
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

        if($1.type == t_ARRAY){
            if($1.info[0] == 'i'){
                fprintf(jFile, "\tiastore\n");
            }
            else if($1.info[0] == 'f'){
                fprintf(jFile, "\tfastore\n");
            }
        }
        else{
            char *t;
            switch($1.type){
                case t_INT: case t_BOOL:
                    t = "i";
                    break;
                case t_FLOAT:
                    t = "f";
                    break;
                case t_STRING:
                    t = "a";
                    break;
                default: break;
            }

            switch($2[0]){
                case 'A':
                    if($2[2] == 'D'){
                        fprintf(jFile, "\t%sadd\n", t);
                    }
                    break;
                case 'S':
                    fprintf(jFile, "\t%ssub\n", t);
                    break;
                case 'M':
                    fprintf(jFile, "\t%smul\n", t);
                    break;
                case 'Q':
                    fprintf(jFile, "\t%sdiv\n", t);
                    break;
                case 'R':
                    fprintf(jFile, "\t%srem\n", t);
                    break;
                default: break;
                
            }

            fprintf(jFile, "\t%sstore %d\n", t, findAddress($1.info));
        }
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
    : Expression IncDecOp {
        char *type;
        char *num;
        switch($1.type){
            case t_INT:
                type = "i";
                num = "1";
                break;
            case t_FLOAT:
                type = "f";
                num = "1.0";
                break;
            default: break;
        }

        fprintf(jFile, "\tldc %s\n"
                       "\t%s%s\n"
                       "\t%sstore %d\n", num, type, $2, type, findAddress($1.info));
    }
;

IncDecOp
    : INC   { 
        $$ = "add";
    }
    | DEC   { 
        $$ = "sub"; 
    }
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
    : IF IfCondition IfBlock Else   {
        fprintf(jFile, "if_section_end_%d:\n", wholeIfSectionCount);
        ++wholeIfSectionCount;
    }
;

IfBlock
    : IfBlockStart NEWLINE StatementList IfBlockEnd
    | IfBlockStart IfBlockEnd
;

IfBlockStart
    : '{'   {
        CreateNewTable(); 
        fprintf(jFile, "\tifeq if_false_flag_%d\n", ifBlockNumOfTime);
    }
;

IfBlockEnd
    : '}'   {
        DeleteNewestTable();
        fprintf(jFile, "\tgoto if_section_end_%d\n", wholeIfSectionCount);
        fprintf(jFile, "if_false_flag_%d:\n\n", ifBlockNumOfTime);
        ++ifBlockNumOfTime;
    }
;

Else
    : 
    | ELSE IfStmt
    | ELSE Block

IfCondition
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
    : ForBegin ForCondition ForConditionBlock    {
        fprintf(jFile, "for_section_exit_%d:\n", forSectionCount);
        ++forSectionCount;    
    }
    | ForBegin ForClause ForClauseBlock    {
        fprintf(jFile, "for_section_exit_%d:\n", forSectionCount);
        ++forSectionCount;
    }
;

ForBegin
   : FOR    { fprintf(jFile, "for_start_%d:\n", forSectionCount); }
;

ForCondition
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

        fprintf(jFile, "\tifeq for_section_exit_%d\n", forSectionCount);
    }
;

ForClause
    : InitStmt ';' ForClauseCondition ';' PostStmt
;

ForClauseCondition
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

        fprintf(jFile, "\tifeq for_section_exit_%d\n"
                       "\tgoto for_loop_body_%d\n"
                       "for_post_statement_%d:\n", forSectionCount, forSectionCount, forSectionCount);
    }
;

ForConditionBlock
    : ForBlockStart NEWLINE StatementList ForBlockEnd   {
        fprintf(jFile, "\tgoto for_start_%d\n", forSectionCount);
    }
    | ForBlockStart ForBlockEnd {
        fprintf(jFile, "\tgoto for_start_%d\n", forSectionCount);
    }
;

ForClauseBlock
    : ForBlockStart NEWLINE StatementList ForBlockEnd   {
        fprintf(jFile, "\tgoto for_post_statement_%d\n", forSectionCount);
    }
    | ForBlockStart ForBlockEnd {
        fprintf(jFile, "\tgoto for_post_statement_%d\n", forSectionCount);
    }
;

ForBlockStart
    : '{'
;

ForBlockEnd
    : '}'
;

InitStmt
    : SimpleStmt    { fprintf(jFile, "for_condition_check_%d:\n", forSectionCount); }
;

PostStmt
    : SimpleStmt    { fprintf(jFile, "\tgoto for_condition_check_%d\n"
                                     "for_loop_body_%d:\n", forSectionCount, forSectionCount); }
;

PrintStmt
    : PRINT '(' Expression ')'      { printInstruction(P, $3.type); }
    | PRINTLN '(' Expression ')'    { printInstruction(PLN, $3.type); }
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

    jFile = fopen("hw3.j", "w");
    jOutputBuffer = malloc(sizeof(char) * 2048);

    fprintf(jFile, ".source hw3.j\n"
                   ".class public Main\n"
                   ".super java/lang/Object\n"
                   ".method public static main([Ljava/lang/String;)V\n"
                   ".limit stack 100\n"
                   ".limit locals 100\n\n");

    yylineno = 0;
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    
    fprintf(jFile, "\treturn\n"
                   ".end method\n");
    fclose(jFile);
    return 0;
}

void create_symbol() {
    return;
}

void insertSymbol(t_entry *e) {
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

void loadNewVariable(t_entry *e){
    switch(parseTypeNameStringToEnum(e->type)){
        case t_INT:
            fprintf(jFile, "\tistore %d\n",e->address);
            break;
        case t_FLOAT:
            fprintf(jFile, "\tfstore %d\n", e->address);
            break;
        case t_BOOL:
            fprintf(jFile, "\tistore %d\n", e->address);
            break;
        case t_STRING:
            fprintf(jFile, "\tastore %d\n", e->address);
            break;
        case t_ARRAY:
            switch(parseTypeNameStringToEnum(e->elementType)){
                case t_INT:
                    fprintf(jFile, "\tnewarray int\n"
                                   "\tastore %d\n", e->address);
                    break;
                case t_FLOAT:
                    fprintf(jFile, "\tnewarray float\n"
                                   "\tastore %d\n", e->address);
                    break;
                default: break;
            }
        default: break;
    }
}

void printInstruction(t_printType pt, t_varType vt){
    char* type;
    switch(vt){
        case t_INT:
            type = "I";
            break;
        case t_FLOAT:
            type = "F";
            break;
        case t_STRING:
            type = "Ljava/lang/String;";
            break;
        case t_BOOL:
            type = "Ljava/lang/String;";
            fprintf(jFile, "\tifne bool_print_%d\n"
                           "\tldc \"false\"\n"
                           "\tgoto bool_print_end_%d\n"
                           "bool_print_%d:\n"
                           "\tldc \"true\"\n"
                           "bool_print_end_%d:\n",
                           boolPrintNumOfTime, boolPrintNumOfTime, boolPrintNumOfTime, boolPrintNumOfTime);
            ++boolPrintNumOfTime;           
            break;
        default: break;
    }

    fprintf(jFile, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n"
                   "\tswap\n"
                   "\tinvokevirtual java/io/PrintStream/");

    switch(pt){
        case P:
            fprintf(jFile, "print(%s)V\n", type);
            break;
        case PLN:
            fprintf(jFile, "println(%s)V\n", type);
    }

    return;
}

void compareInstruction(t_varType type, char* jump_inst){
    char *t;
    switch(type){
        case t_INT:
            t = "i";
            break;
        case t_FLOAT:
            t = "f";
            break;
        default: break;
    }

    if(!strcmp(t, "f")){
        fprintf(jFile, "\tfcmpl\n");
    }
    else{
        fprintf(jFile, "\t%ssub\n", t);
    }
    fprintf(jFile, "\t%s comp_true_%d\n"
                   "\ticonst_0\n"
                   "\tgoto comp_end_%d\n"
                   "comp_true_%d:\n"
                   "\ticonst_1\n"
                   "comp_end_%d:\n", jump_inst, compNumOfTime, compNumOfTime, compNumOfTime, compNumOfTime);
    ++compNumOfTime;
}