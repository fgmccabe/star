#ifndef _TYPE_P_H_
#define _TYPE_P_H_

#include "type.h"

typedef struct _typeVar_object_ {
  char *name;
  typeTuplePo constraints;
} TypeVarO;

typedef struct _typeExp_object_ {
  char *name;
  typeTuplePo args;
} TypeExpO;

typedef struct _arrow_type_object_ {
  typeTuplePo args;
  typePo res;
  typeTuplePo throws;
} ArrowTypeO;

typedef struct _type_attributes_object_ {
  typeTuplePo tpl;
  char **names;
} RecordTypeO;

typedef struct _type_ {
  TypeKind kind;
  union{
    TypeVarO tVar;
    TypeExpO te;
    ArrowTypeO ar;
    RecordTypeO rec;
  } t;
} TypeObject; 

typedef struct _type_tuple_ {
  int size;
  int count;
  typePo *els;
} TypeTupleRec;

#endif
