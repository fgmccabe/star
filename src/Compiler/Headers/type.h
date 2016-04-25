/*
 * define public aspects of type
 */
#ifndef _TYPE_H_
#define _TYPE_H_

#include <logical.h>
#include <unicode.h>
#include "meta.h"
#include "pp.h"

extern uniChar *rawIntType;
extern uniChar *rawLongType;
extern uniChar *rawFloatType;
extern uniChar *rawCharType;
extern uniChar *rawStringType;
extern uniChar *BOOL_TYPE;
extern uniChar *ARROW_TYPE;
extern uniChar *TUPLE_TYPE;
extern uniChar *VOID_TYPE;
extern uniChar *DICT_TYPE;

extern logical isRawIntType(sxPo type);
extern logical isRawLongType(sxPo type);
extern logical isRawFloatType(sxPo type);
extern logical isRawCharType(sxPo type);
extern logical isRawStringType(sxPo type);
extern logical isRawFileType(sxPo type);
extern logical isRawType(sxPo type);
extern logical isVoidType(sxPo type);

typedef enum { TypeVar,
	       TypeExp,
	       ArrowType
} TypeKind;

extern void initTypes();

extern sxPo voidType,booleanType,
  intType,longType,floatType,charType,stringType,
  fileCafeType,errorType,dictType;
extern sxPo voidCon;

extern TypeKind tpKind(sxPo type);

extern logical isTypeVar(sxPo type);
extern sxPo sxTypeVar(locationPo loc,uniChar *name);
extern uniChar *tpVarName(sxPo t);

extern sxPo sxAllType(locationPo loc,lxPo argTypes,sxPo resType);
extern sxPo sxTypeExp(locationPo loc,sxPo type, sxPo argAtype);
extern sxPo sxTypeFun(locationPo loc,sxPo op,lxPo args);
extern sxPo sxTypeOp(sxPo tp);
extern logical isTypeExp(sxPo t);
extern logical isTypeFun(sxPo t);
extern uniChar *typeExpName(sxPo tp);
extern lxPo sxTypeArgs(sxPo tp);
extern sxPo sxTypeDef(locationPo loc,sxPo type);
extern logical sxIsTypeDef(sxPo sx);
extern sxPo sxTypeDefType(sxPo sx);
extern lxPo sxTypeDefCons(sxPo sx);
extern sxPo sxEnumDef(locationPo loc,sxPo name,sxPo args,sxPo type);
extern sxPo sxStructDef(locationPo loc,sxPo name,sxPo args,sxPo type);
extern sxPo sxField(locationPo loc,sxPo name,sxPo type);
extern sxPo sxTupleType(locationPo loc,lxPo fields);
extern sxPo sxRecordType(locationPo loc,lxPo fields);
extern sxPo sxArrowType(locationPo loc,sxPo argType,sxPo resType);
extern logical isArrowType(sxPo type);
extern sxPo arrowArgType(sxPo type);
extern sxPo arrowResType(sxPo type);
extern sxPo arrowThrows(sxPo type);
extern sxPo arrowtypeArg(sxPo type,int ix);

extern sxPo sxPttrnType(locationPo loc,sxPo argType, sxPo ptnType);
extern logical isPttrnType(sxPo type);
extern sxPo ptnArgType(sxPo type);
extern sxPo ptnPtnType(sxPo type);

extern retCode checkType(sxPo actual, sxPo *expected,locationPo loc);

extern retCode dispType(ppDisplayPo disp,policyPo pol,sxPo ab);

#endif
