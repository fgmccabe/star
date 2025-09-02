//
// Created by Francis McCabe on 8/23/25.
//

#ifndef STAR_ABORT_H
#define STAR_ABORT_H

typedef enum {
  successCode = 0, /* Normal exit */
  failCode, /* Failing exit */
  errorCode,  // Error exit
  oomCode, // Out of memory
  undefinedCode, // tried to execute undefined code
  fiberCode, // Something wrong with fiber code
  singleCode, // something wrong with single assignment var
  assignmentCode, // something wrong with assignment
  specialMethodCode, // something wrong with a special method
  invalidOperationCode, // tried an invalid operation
  abortCode, // abort operation
} ExitCode;

void star_exit(ExitCode code);

#endif //STAR_ABORT_H
