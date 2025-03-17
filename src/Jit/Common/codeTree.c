//
// Created by Francis McCabe on 3/14/25.
//

#include "codeTreeP.h"
#include "codeP.h"

static poolPo nodePool = Null;

void initCodeTree(){
  if(nodePool==Null){
    nodePool = newPool(sizeof(CodeNodeRecord),1024);
  }
}

codeTreePo scanBlock(insPo code,int32 start,int32 end){

}
