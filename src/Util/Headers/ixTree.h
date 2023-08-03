/*
  Interface to indextree
  Applicative tree with performance near hash tree

  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _IXTREE_H
#define _IXTREE_H

#include "config.h"
#include "retcode.h"
#include "logical.h"
#include "integer.h"
#include "object.h"
#include "unistr.h"

typedef struct _ix_tree_ *treePo;
extern classPo treeClass;

extern logical ixContains(treePo tree, objectPo key);
extern logical ixIsEmpty(treePo tree);
extern long ixSize(treePo tree);

extern treePo ixInsert(treePo tree, objectPo key, objectPo val);
extern treePo ixDelete(treePo tree, objectPo key);

extern treePo ixEmptyTree();

extern treePo ixLeafTree(objectPo key, objectPo val);

typedef retCode (*treeProc)(objectPo key, void *val, void *cl);

extern retCode ixIterate(treePo tree, treeProc proc, void *cl);

typedef void *(*ixFolder)(objectPo key, objectPo value, void *state);
extern void *ixFold(treePo tree, ixFolder f, void *init);


#endif //_IXTREE_H
