#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct Entry{
    int index;
    char *name;
    char *type;
    int address;
    int lineNo;
    char* elementType;   
    struct Entry* next;
}t_entry;

typedef struct Table{
    int level;
    int nextIndex;
    int tableSize;
    t_entry *head;
    struct Table *next;
}t_table;

typedef enum VarTypes{
    t_INT, t_FLOAT, t_BOOL, t_STRING, t_ARRAY, t_NONE, t_ERROR
}t_varType;

struct OperandInfo{
    char *info;
    enum VarTypes type;    
};

struct TypeInfo{
    char *element_type;
    char *type_name;
};


#endif /* COMMON_H */