/*
  Return code enumeration
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _RET_CODE_H_
#define _RET_CODE_H_

typedef enum {
  Ok, Fail, Switch, Suspend, Interrupt, Error, Eof, Space
} retCode;

typedef enum {
  smaller, /* One item is smaller than another */
  same, /* Two items are the same */
  bigger, /* One it bigger than the other */
  incomparible                  /* Incomparible */
} comparison;

#define tryRet(Exp) do{ retCode ret=(Exp); if(ret!=Ok)return ret; }while(False)

#endif
